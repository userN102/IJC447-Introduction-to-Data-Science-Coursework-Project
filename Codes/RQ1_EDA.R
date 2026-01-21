
# RQ1:
# How do enterprise births vary across UK industries between 2019 and 2023, 
# and how are these differences associated with industry size, 
# enterprise survival, and high-growth activity?
# Output: Figures saved to ./Outputs/Visuals/


library(tidyverse)
library(scales)
library(corrplot)


if (!dir.exists("./Outputs/Visuals")) dir.create("./Outputs/Visuals", recursive = TRUE)


# Load data 
master_panel <- read_csv(
  "./Dataset/Final_Master_Datasets/master_panel_industry_characteristics_2019_2023.csv"
)

# Quick data audit (structure + missingness)
glimpse(master_panel)
summary(master_panel)
colSums(is.na(master_panel))


# Ensure consistent types for grouping and labels
master_panel <- master_panel %>%
  mutate(
    year = as.integer(year),
    industry_code = as.factor(industry_code),
    industry_name = as.character(industry_name)  # keep for labels or plots
    
  )

# Structure check
master_panel %>%
  summarise(
    n_rows = n(),
    n_industries = n_distinct(industry_code),
    years = paste(sort(unique(year)), collapse = ", ")
  )

# Create EDA features (raw + transformed) 
# Keep raw variables for interpretation and log1p versions for skewed counts
eda <- master_panel %>%
  mutate(
    industry_name_wrapped = stringr::str_wrap(as.character(industry_name), width = 28),
    births = births_of_new_enterprises,
    active = active_enterprises,
    active_10plus = active_enterprises_10plus_employees,
    deaths = deaths_of_new_enterprises,
    high_growth = high_growth_enterprises,
    survival_1yr = percent_of_1_year,
    survival_2yr = percent_of_2_year,
    survival_3yr = percent_of_3_year,
    survival_4yr = percent_of_4_year,
    survival_5yr = percent_of_5_year,
    
    # Births per 1,000 active enterprises (controls for industry size)
    birth_intensity = if_else(
      active > 0,
      (births / active) * 1000,
      NA_real_
    ),
    log_births = log1p(births),
    log_active = log1p(active),
    log_active_10plus = log1p(active_10plus),
    log_deaths = log1p(deaths),
    log_high_growth = log1p(high_growth)
    )


# Raw distributions
p_births_raw <- ggplot(eda, aes(births)) +
  geom_histogram(bins = 30) +
  labs(title = "Enterprise births (raw)", x = "Births", y = "Frequency")


# Figure 2.Distribution of enterprise births across UK industries (2019–2023). 
p_births_raw
ggsave("./Outputs/Visuals/RQ1_births_raw_distribution.png", p_births_raw, width = 8, height = 5, dpi = 300)


p_active_raw <- ggplot(eda, aes(active)) +
  geom_histogram(bins = 30) +
  labs(title = "Active enterprises (raw)", x = "Active enterprises", y = "Frequency")

# Figure 3.Distribution of active enterprises across UK industries (2019–2023). 
p_active_raw
ggsave("./Outputs/Visuals/RQ1_active_enterprises_raw_distribution.png", p_active_raw, width = 8, height = 5, dpi = 300)


p_active_10plus_raw <- ggplot(eda, aes(active_10plus)) +
  geom_histogram(bins = 30) +
  labs(title = "Active enterprises (10+ employees) (raw)",
       x = "Active enterprises (10+)", y = "Frequency")


p_active_10plus_raw
ggsave("./Outputs/Visuals/RQ1_active_enterprises_10plus_raw_distribution.png", p_active_10plus_raw, width = 8, height = 5, dpi = 300)


p_deaths_raw <- ggplot(eda, aes(deaths)) +
  geom_histogram(bins = 30) +
  labs(title = "Enterprise deaths (raw)", x = "Deaths", y = "Frequency")


# Figure 4.Distribution of enterprise deaths across UK industries (2019–2023). 
p_deaths_raw
ggsave("./Outputs/Visuals/RQ1_deaths_raw_distribution.png", p_deaths_raw, width = 8, height = 5, dpi = 300)



p_hg_raw <- ggplot(eda, aes(high_growth)) +
  geom_histogram(bins = 30) +
  labs(title = "High-growth enterprises (raw)", x = "High-growth enterprises", y = "Frequency")

# Figure 5.Distribution of high-growth enterprises across UK industries (2019–2023). 
p_hg_raw
ggsave("./Outputs/Visuals/RQ1_high_growth_raw_distribution.png", p_hg_raw , width = 8, height = 5, dpi = 300)


# Log-transformed distributions (skew reduction)
p_births_log <- ggplot(eda, aes(log_births)) +
  geom_histogram(bins = 30) +
  labs(title = "Enterprise births (log1p)", x = "log(1 + births)", y = "Frequency")

# Figure 6.Distribution of enterprise births after log(1+x) transformation (2019–2023).
p_births_log 
ggsave("./Outputs/Visuals/RQ1_births_log_distribution.png", p_births_log, width = 8, height = 5, dpi = 300)


p_active_log <- ggplot(eda, aes(log_active)) +
  geom_histogram(bins = 30) +
  labs(title = "Active enterprises (log1p)", x = "log(1 + active)", y = "Frequency")


# Figure 7.Distribution of active enterprises after log(1+x) transformation (2019–2023). 
p_active_log 
ggsave("./Outputs/Visuals/RQ1_active_log_distribution.png", p_active_log, width = 8, height = 5, dpi = 300)


p_deaths_log <- ggplot(eda, aes(log_deaths)) +
  geom_histogram(bins = 30) +
  labs(title = "Enterprise deaths (log1p)", x = "log(1 + deaths)", y = "Frequency")

# Figure 8.Distribution of enterprise deaths after log(1+x) transformation (2019–2023). 
p_deaths_log 
ggsave("./Outputs/Visuals/RQ1_deaths_log_distribution.png", p_deaths_log, width = 8, height = 5, dpi = 300)



p_active_10plus_log <- ggplot(eda, aes(log_active_10plus)) +
  geom_histogram(bins = 30) +
  labs(title = "Active enterprises with 10+ employees (log1p)", x = "log(1 + active_10plus)", y = "Frequency")

p_active_10plus_log 
ggsave("./Outputs/Visuals/RQ1_active_10plus_log_distribution.png", p_active_10plus_log, width = 8, height = 5, dpi = 300)


p_high_growth_log <- ggplot(eda, aes(log_high_growth)) +
  geom_histogram(bins = 30) +
  labs(title = "High-growth enterprises (log1p)", x = "log(1 + high-growth)", y = "Frequency")


# Figure 9.Distribution of high-growth enterprises after log(1+x) transformation (2019–2023). 
p_high_growth_log 
ggsave("./Outputs/Visuals/RQ1_high_growth_log_distribution.png", p_high_growth_log, width = 8, height = 5, dpi = 300)


# Size-adjusted entry : Birth intensity 
# Figure 10.Average number of enterprise births per 1,000 active enterprises across UK industries, 2019–2023. 

p_birth_intensity_time <- eda %>%
  group_by(year) %>%
  summarise(mean_birth_intensity = mean(birth_intensity, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(year, mean_birth_intensity)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Mean birth intensity over time (2019–2023)",
    x = "Year",
    y = "Mean births per 1,000 active enterprises"
  )

p_birth_intensity_time
ggsave("./Outputs/Visuals/RQ1_birth_intensity_over_time.png",
       p_birth_intensity_time, width = 8, height = 5, dpi = 300)


# Figure 12.Birth intensity over time for selected UK industries, 2019–2023. 

top_industries <- eda %>%
  group_by(industry_code) %>%
  summarise(total_births = sum(births, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_births)) %>%
  slice_head(n = 10) %>%
  pull(industry_code)

p_birth_intensity_top_industries <- eda %>%
  filter(industry_code %in% top_industries, is.finite(birth_intensity)) %>%
  group_by(year, industry_code, industry_name_wrapped) %>%
  summarise(mean_birth_intensity = mean(birth_intensity, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(year, mean_birth_intensity, color = industry_name_wrapped)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Birth intensity over time for top industries (2019–2023)",
    x = "Year",
    y = "Births per 1,000 active enterprises",
    color = "Industry"
  )

p_birth_intensity_top_industries
ggsave("./Outputs/Visuals/RQ1_birth_intensity_top_industries.png",
       p_birth_intensity_top_industries, width = 9, height = 6, dpi = 300)


# Industry comparisons (differences across industries) 
# Figure 11.Distribution of enterprise births across selected UK industries, 2019–2023. 

p_births_box_top <- eda %>%
  filter(industry_code %in% top_industries) %>%
  ggplot(aes(reorder(industry_name_wrapped, births, FUN = median), births)) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "Distribution of births across top industries (2019–2023)",
    x = "Industry",
    y = "Births"
  )

p_births_box_top
ggsave("./Outputs/Visuals/RQ1_births_distribution_by_industry.png",
       p_births_box_top, width = 9, height = 6, dpi = 300)


# Associations (births vs size, survival, high-growth) 
# Scatterplots with trend lines
# These plots show patterns and they do not establish causality.

# Figure 13.Association between birth and industry size. 
p_births_vs_size <- ggplot(eda, aes(log_active, log_births)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Association: births vs industry size",
    x = "log(1 + active enterprises)",
    y = "log(1 + births)"
  )

p_births_vs_size
ggsave("./Outputs/Visuals/RQ1_births_vs_industry_size.png",
       p_births_vs_size, width = 8, height = 5, dpi = 300)


# Figure 14.Association between births and high-growth enterprise activity.
p_births_vs_high_growth <- ggplot(eda, aes(log_high_growth, log_births)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Association: births vs high-growth activity",
    x = "log(1 + high-growth enterprises)",
    y = "log(1 + births)"
  )

p_births_vs_high_growth
ggsave("./Outputs/Visuals/RQ1_births_vs_high_growth.png",
       p_births_vs_high_growth, width = 8, height = 5, dpi = 300)


# Figure 15.Association between birth and one-year survival rate.
p_births_vs_survival <- ggplot(eda, aes(survival_1yr, log_births)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Association: births vs 1-year survival rate",
    x = "1-year survival (%)",
    y = "log(1 + births)"
  )

p_births_vs_survival
ggsave("./Outputs/Visuals/RQ1_births_vs_survival_1yr.png",
       p_births_vs_survival, width = 8, height = 5, dpi = 300)



p_births_vs_deaths <- ggplot(eda, aes(log_deaths, log_births)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Association: births vs deaths (context for churn)",
    x = "log(1 + deaths)",
    y = "log(1 + births)"
  )

p_births_vs_deaths
ggsave("./Outputs/Visuals/RQ1_births_vs_deaths.png",
       p_births_vs_deaths, width = 8, height = 5, dpi = 300)



# Correlation analysis (feature selection justification) 

# Figure 16.Correlation between enterprise survival rates across time horizons. 
survival_only <- eda %>%
  select(survival_1yr, survival_2yr, survival_3yr, survival_4yr, survival_5yr)

survival_cor <- cor(survival_only, use = "pairwise.complete.obs")
round(survival_cor, 2)

png("./Outputs/Visuals/RQ1_survival_measures_correlation.png",
    width = 1200, height = 900, res = 150)

corrplot(
  survival_cor,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  number.cex = 0.7
)

dev.off()

# Correlation among predictors (justifies dropping redundant predictors)

# Figure 17.Correlation matrix of key predictors used in modelling. 
predictor_corr <- eda %>%
  select(
    log_active,
    log_active_10plus,
    log_deaths,
    log_high_growth,
    survival_1yr
  )

pred_cor <- cor(predictor_corr, use = "pairwise.complete.obs")
round(pred_cor, 2)

png("./Outputs/Visuals/RQ1_predictor_correlation_matrix.png",
    width = 1200, height = 900, res = 150)

corrplot(
  pred_cor,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  number.cex = 0.7
)

dev.off()

target_assoc <- eda %>%
  select(
    log_births,
    log_active,
    log_active_10plus,
    log_deaths,
    log_high_growth,
    survival_1yr
  )

round(cor(target_assoc, use = "pairwise.complete.obs"), 2)


