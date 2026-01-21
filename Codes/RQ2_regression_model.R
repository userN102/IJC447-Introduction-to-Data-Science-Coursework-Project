
# RQ2: 
# How accurately can enterprise births be predicted using industry-level business demography indicators, 
# and how does the predictive performance of multiple linear regression compare with random forest regression 
# when evaluated using cross-validation on a limited dataset?

# - Models use the lagged predictors (t-1) to mimic forecasting.
# - Baseline = naive lag (predict births_t using births_(t-1)).

# Output:
# - Metrics + predictions: ./Outputs/Tables/
# - Figures: ./Outputs/Visuals/

library(tidyverse)
library(tidymodels)
library(ranger)
library(rsample)

set.seed(123)


# Output folders

if (!dir.exists("./Outputs/Tables")) dir.create("./Outputs/Tables", recursive = TRUE)
if (!dir.exists("./Outputs/Visuals")) dir.create("./Outputs/Visuals", recursive = TRUE)

# Load data + build lagged dataset

master_panel <- read_csv(
  "./Dataset/Final_Master_Datasets/master_panel_industry_characteristics_2019_2023.csv"
)

# Create target and same-year variables to build lags
model_data <- master_panel %>%
  mutate(
    year = as.integer(year),
    industry_code = as.factor(industry_code),
    
    # Target (log scale reduces skew in birth counts)
    log_births = log1p(births_of_new_enterprises),
    
    # Same-year versions (used only to compute t-1 lags)
    log_active = log1p(active_enterprises),
    log_high_growth = log1p(high_growth_enterprises),
    survival_1yr = percent_of_1_year
  ) %>%
  arrange(industry_code, year) %>%
  group_by(industry_code) %>%
  mutate(
    # Lag predictors (t-1)
    lag_log_active      = lag(log_active, 1),
    lag_log_high_growth = lag(log_high_growth, 1),
    lag_survival_1yr    = lag(survival_1yr, 1),
    # Baseline input (lagged target)
    lag_log_births      = lag(log_births, 1)
  ) %>%
  ungroup() %>%
  drop_na(
    year, industry_code, log_births,
    lag_log_active, lag_log_high_growth, lag_survival_1yr,
    lag_log_births
  ) %>%
  arrange(year) %>%
  mutate(
    # Stable observation identifier for checking joins and outputs
    obs_id = paste0(as.character(industry_code), "_", year)
  )


nrow(model_data) == n_distinct(model_data$obs_id)

# Each (industry, year) must be unique as joins and CV depend on it.
stopifnot(nrow(model_data) == n_distinct(model_data$obs_id))



glimpse(model_data)
print(sort(unique(model_data$year)))


# Lag Verification (against leakage)
# Confirms lag variables reference the previous year within each industry.

cat("\n VERIFYING LAG CONSTRUCTION \n")

lag_verification <- model_data %>%
  arrange(industry_code, year) %>%
  group_by(industry_code) %>%
  mutate(
    lag_check_active      = near(lag_log_active,      lag(log_active, 1)),
    lag_check_high_growth = near(lag_log_high_growth, lag(log_high_growth, 1)),
    lag_check_survival    = near(lag_survival_1yr,    lag(survival_1yr, 1)),
    year_gap = year - lag(year, 1)
  ) %>%
  ungroup() %>%
  filter(!is.na(year_gap)) %>%
  summarise(
    all_lags_correct = all(lag_check_active & lag_check_high_growth & lag_check_survival),
    pct_correct = mean(lag_check_active & lag_check_high_growth & lag_check_survival) * 100,
    all_consecutive = all(year_gap == 1, na.rm = TRUE),
    n_gaps = sum(year_gap != 1, na.rm = TRUE)
    
  )

print(lag_verification)

if (isTRUE(lag_verification$all_lags_correct)) {
  cat("Lag features correctly reference the previous year within each industry.\n")
  if (!isTRUE(lag_verification$all_consecutive)) {
    warning("Some industries have non-consecutive years after filtering (n_gaps > 0). Interpret results cautiously.")
  }
} else {
  stop("Lag verification FAILED: lag variables do not match previous-year values.")
}


# Drop same-year predictors to avoid accidental leakage
model_data <- model_data %>%
  select(-log_active, -log_high_growth, -survival_1yr)

# Time-aware cross-validation (manual forward-chaining CV)
# 2 realistic evaluations:
# - Train 2020–2021 -> Test 2022
# - Train 2020–2022 -> Test 2023

model_data <- model_data %>%
  mutate(year = as.integer(year)) %>%
  arrange(year, industry_code) %>%
  mutate(.row = row_number())


print(sort(unique(model_data$year)))

make_year_split <- function(data, train_years, test_year, id_label) {
  analysis_idx <- which(data$year %in% train_years)
  assess_idx   <- which(data$year == test_year)
  
  if (length(analysis_idx) == 0) stop("No training rows for: ", id_label)
  if (length(assess_idx) == 0) stop("No test rows for: ", id_label)
  
  rsample::make_splits(list(analysis = analysis_idx, assessment = assess_idx), data = data) %>%
    list(splits = ., id = id_label)
}

split_list <- list(
  make_year_split(
    data = model_data,
    train_years = c(2020, 2021),
    test_year   = 2022,
    id_label    = "Train_2020_2021_Test_2022"
  ),
  make_year_split(
    data = model_data,
    train_years = c(2020, 2021, 2022),
    test_year   = 2023,
    id_label    = "Train_2020_2022_Test_2023"
  )
)

folds <- rsample::manual_rset(
  splits = purrr::map(split_list, "splits"),
  ids    = purrr::map_chr(split_list, "id")
)

folds

# Print fold structure
purrr::iwalk(folds$splits, function(s, i) {
  cat("\n  ", folds$id[i], "  \n")
  cat("Train years:", paste(sort(unique(rsample::analysis(s)$year)), collapse = ", "), "\n")
  cat("Test years :", paste(sort(unique(rsample::assessment(s)$year)), collapse = ", "), "\n")
  cat("Train n =", nrow(rsample::analysis(s)), "| Test n =", nrow(rsample::assessment(s)), "\n")
})

# Shared preprocessing 

rec_shared <- recipe(
  log_births ~ lag_log_active + lag_log_high_growth + lag_survival_1yr,
  data = model_data
) %>%
  step_zv(all_predictors()) %>%   # remove predictors with zero variance
  step_lincomb(all_predictors())  # remove perfect linear combinations



# Determine predictor count for Random Forest tuning after preprocessing
prep_rec <- prep(rec_shared, training = model_data)
n_pred <- ncol(juice(prep_rec)) - 1
message("Number of predictors after preprocessing: ", n_pred)


# Baseline evaluation (Naive lag births)
# Baseline predicts births_t using births_(t-1).
# I compute baseline metrics per fold (same test years).


# Baseline prediction: log_births_t ≈ log_births_(t-1)
calc_metrics <- function(df, truth, estimate) {
  yardstick::metric_set(yardstick::rmse, yardstick::rsq, yardstick::mae)(
    df, truth = {{ truth }}, estimate = {{ estimate }}
  )
}


baseline_fold_metrics <- purrr::imap_dfr(folds$splits, function(s, i) {
  test_df <- rsample::assessment(s)
  tmp <- test_df %>% transmute(year, log_births, .pred = lag_log_births)
  calc_metrics(tmp, truth = log_births, estimate = .pred) %>%
    mutate(id = folds$id[i])
})


baseline_metrics <- baseline_fold_metrics %>%
  group_by(.metric) %>%
  summarise(
    mean = mean(.estimate),
    std_err = sd(.estimate) / sqrt(n()),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(model = "Naive lag baseline") %>%
  select(model, .metric, mean, std_err, n)


# Linear Regression (time-aware CV)

lm_spec <- linear_reg() %>%
  set_engine("lm", rankdeficient = "NA")

lm_wf <- workflow() %>%
  add_recipe(rec_shared) %>%
  add_model(lm_spec)

lm_cv <- fit_resamples(
  lm_wf,
  resamples = folds,
  metrics = yardstick::metric_set(yardstick::rmse, yardstick::rsq, yardstick::mae),
  control = control_resamples(save_pred = TRUE)
)

names(collect_predictions(lm_cv))


lm_metrics <- collect_metrics(lm_cv) %>%
  mutate(model = "Linear regression (lag-only)")



# Random Forest (tuned, time-aware CV)
prep_rec <- prep(rec_shared, training = model_data)
n_pred <- ncol(juice(prep_rec)) - 1

rf_spec <- rand_forest(
  trees = 300,
  mtry  = tune(),
  min_n = tune()
) %>%
  set_engine("ranger", importance = "permutation") %>%
  set_mode("regression")

rf_wf <- workflow() %>%
  add_recipe(rec_shared) %>%
  add_model(rf_spec)


rf_grid <- grid_regular(
  mtry(range = c(2, max(2, n_pred))),
  min_n(range = c(5, 15)),
  levels = 2
)

rf_tune <- tune_grid(
  rf_wf,
  resamples = folds,
  grid = rf_grid,
  metrics = metric_set(yardstick::rmse, yardstick::rsq, yardstick::mae),
  control = control_grid(save_pred = TRUE)
)

best_rf <- select_best(rf_tune, metric = "rmse")


rf_final_wf <- finalize_workflow(rf_wf, best_rf)

rf_cv_final <- fit_resamples(
  rf_final_wf,
  resamples = folds,
  metrics = metric_set(yardstick::rmse, yardstick::rsq, yardstick::mae),
  control = control_resamples(save_pred = TRUE)
)

rf_metrics <- collect_metrics(rf_cv_final) %>%
  mutate(model = "Random forest (tuned, lag-only)")


rf_metrics_by_fold <- collect_metrics(rf_cv_final, summarize = FALSE) %>%
  mutate(model = "Random forest (tuned, lag-only)")



# Export metrics (baseline + LM + RF)

# Table 1.Time-aware cross-validation performance of linear regression, random forest and a naïve lagged-births baseline. 

metrics_table <- bind_rows(
  baseline_metrics,
  lm_metrics %>% rename(mean = mean, std_err = std_err, n = n) %>% select(model, .metric, mean, std_err, n),
  rf_metrics %>% rename(mean = mean, std_err = std_err, n = n) %>% select(model, .metric, mean, std_err, n)
) %>%
  arrange(.metric, model)

write_csv(metrics_table, "./Outputs/Tables/RQ2_timeCV_metrics_with_baseline_rmse_r2_mae.csv")


# Collect predictions (all models) for plots and analysis

lm_raw <- collect_predictions(lm_cv, summarize = FALSE)

lm_preds <- lm_raw %>%
  left_join(
    model_data %>% transmute(.row, obs_id, year, log_births_true = log_births, lag_log_births),
    by = ".row"
  ) %>%
  mutate(model = "Linear regression (lag-only)")

rf_raw <- collect_predictions(rf_cv_final, summarize = FALSE)

rf_preds <- rf_raw %>%
  left_join(
    model_data %>% transmute(.row, obs_id, year, log_births_true = log_births, lag_log_births),
    by = ".row"
  ) %>%
  mutate(model = "Random forest (tuned, lag-only)")


test_rows <- bind_rows(
  lm_raw %>% select(.row),
  rf_raw %>% select(.row)
) %>%
  distinct()

baseline_preds <- model_data %>%
  semi_join(test_rows, by = ".row") %>%
  transmute(
    .row,
    obs_id,
    year,
    log_births_true = log_births,
    lag_log_births,
    .pred = lag_log_births,
    model = "Naive lag baseline"
  )



cv_preds_all <- bind_rows(
  lm_preds %>% select(.row, obs_id, year, log_births_true, lag_log_births, .pred, model),
  rf_preds %>% select(.row, obs_id, year, log_births_true, lag_log_births, .pred, model),
  baseline_preds %>% select(.row, obs_id, year, log_births_true, lag_log_births, .pred, model)
) %>%
  mutate(
    observed  = log_births_true,
    predicted = .pred,
    residual  = observed - predicted,
    test_year = year
  )


count(cv_preds_all, model, test_year)


# Diagnostic plots (include baseline)


# Figure 18.Observed versus predicted enterprise births under time-aware cross-validation.
p_obs_pred <- ggplot(cv_preds_all, aes(x = observed, y = predicted)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(model ~ test_year) +
  labs(
    title = "Time-aware CV: Observed vs Predicted (log births)",
    x = "Observed log(1 + births)",
    y = "Predicted log(1 + births)"
  )

ggsave(
  "./Outputs/Visuals/RQ2_timeCV_observed_vs_predicted.png",
  p_obs_pred, width = 12, height = 8, dpi = 300
)

# Figure 19.Residuals versus predicted values for enterprise births under time-aware cross-validation (log scale), by model 
# and test year.
p_resid_pred <- ggplot(cv_preds_all, aes(x = predicted, y = residual)) +
  geom_point(alpha = 0.35) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_grid(model ~ test_year) +
  labs(
    title = "Time-aware CV: Residuals vs Predicted (log births)",
    x = "Predicted log(1 + births)",
    y = "Residual (observed - predicted)"
  )

ggsave(
  "./Outputs/Visuals/RQ2_timeCV_residuals_vs_predicted.png",
  p_resid_pred, width = 12, height = 8, dpi = 300
)



# Back-transform (interpretation in original scale)

# Figure 20.Observed versus predicted enterprise births on the original scale under time-aware cross-validation, by model 
# and test year. 
cv_preds_bt <- cv_preds_all %>%
  mutate(
    observed_births  = expm1(observed),
    predicted_births = expm1(predicted)
  )

p_obs_pred_bt <- ggplot(cv_preds_bt, aes(x = observed_births, y = predicted_births)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(model ~ test_year, scales = "free") +
  labs(
    title = "Time-aware CV: Observed vs Predicted (births, back-transformed)",
    x = "Observed births",
    y = "Predicted births"
  )

ggsave(
  "./Outputs/Visuals/RQ2_timeCV_observed_vs_predicted_births_backtransformed.png",
  p_obs_pred_bt, width = 12, height = 8, dpi = 300
)


# Reproducibility
write_lines(capture.output(sessionInfo()), "./Outputs/Tables/sessionInfo_RQ2.txt")
