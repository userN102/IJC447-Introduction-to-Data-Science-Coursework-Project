
# IJC447-Introduction-to-Data-Science-Coursework-Project

# Enterprise Entry, Survival, and Growth across UK Industries (2019–2023)

This project analyses UK industry-level business demography data (ONS) to understand how **enterprise births** differ across industries and time. It combines cleaned ONS tables into a single master panel, uses **exploratory data analysis (EDA)** to describe patterns and relationships, and then evaluates **forecast-style prediction models** using lagged indicators and time-aware cross-validation.

## Research Questions

**RQ1:** How do enterprise births vary across UK industries between 2019 and 2023, and how are these differences associated with industry size, enterprise survival, and high-growth activity?

**RQ2:** How accurately can enterprise births be predicted using industry-level business demography indicators, and how does the predictive performance of multiple linear regression compare with random forest regression when evaluated using cross-validation on a limited dataset?

## Key Findings (Summary)

* Enterprise births are **highly uneven across industries**: a small number of industries account for a large share of total births, and distributions are strongly **right-skewed**.
* Differences remain even after adjusting for size using **birth intensity** (births per 1,000 active enterprises), suggesting entry patterns are **not explained by scale alone**.
* In bivariate patterns, births tend to be more strongly associated with **industry size** and **high-growth activity** than with short-term survival rates.
* For prediction, a **naive lagged-births baseline** performs strongly on typical errors (MAE), while **random forest** can improve performance for larger errors (RMSE). Overall gains over simple baselines are modest given the short panel(2019-2023) and only two test folds.

---

## Repository Structure
## Repository Structure

```text
Codes/
├── active_enterprises_10_employer_industry.R
├── active_enterprises_industry.R
├── enterprise_births_industry.R
├── enterprise_deaths_industry.R
├── enterprise_survival_industry.R
├── high_growth_enterprises_industry.R
├── merge_industry_characteristics.R
├── RQ1_EDA.R
└── RQ2_regression_model.R

Dataset/
├── ONS_Business_Demography/
│   ├── ons_original.xlsx
│   ├── Births_Of_New_Enterprises_2019_2024_by_Industry.xlsx
│   ├── Deaths_Of_New_Enterprises_2019_2024_by_Industry.xlsx
│   ├── Active_Enterprises_2019_2024_by_Industry.xlsx
│   ├── Active_Enterprises_10+_Emp_2019_2024_by_Industry.xlsx
│   ├── High_Growth_Enterprises_2019_2024_by_Industry.xlsx
│   └── Births_and_Survival_of_Enterprises_2019_2023_by_Industry.xlsx
└── Final_Master_Datasets/
    └── master_panel_industry_characteristics_2019_2023.xlsx

Outputs/
├── Tables/
└── Visuals/

Workflow_Diagram/
└── workflow.png


A workflow diagram summarising the full data pipeline is provided in the Workflow Diagram/ folder.

## R Code

### 1) Data extraction (from the ONS Excel workbook)

These scripts read ONS tables and save tidy CSV outputs (long format unless noted):

* `enterprise_births_industry.R` → births of new enterprises by industry/year (Table 1.2)
* `enterprise_deaths_industry.R` → deaths of new enterprises by industry/year (Table 2.2)
* `active_enterprises_industry.R` → active enterprises by industry/year (Table 3.2)
* `high_growth_enterprises_industry.R` → high-growth enterprises by industry/year (Table 7.2)
* `active_enterprises_10_employer_industry.R` → active enterprises (10+ employees) by industry/year (Table 7.4)
* `enterprise_survival_industry.R` → births + survival (1–5 years) by industry/year (Table 5.2a–5.2e)


### 2) Build master panel (merge all indicators)

* `merge_industry_characteristics.R`
  Reads the cleaned CSV files and merges them into:
  `Dataset/Final_Master_Datasets/master_panel_industry_characteristics_2019_2023.csv`


### 3) EDA (RQ1)

* `RQ1_EDA.R`
  Produces distributions, time patterns, industry comparisons, association plots, and correlation matrices to justify feature choices for modelling.

### 4) Modelling (RQ2)

* `RQ2_regression_model.R`
  Builds lagged predictors (t-1), runs time-aware cross-validation, compares:

  * **Naive lagged-births baseline**
  * **Multiple linear regression (lag-only)**
  * **Random forest regression (lag-only, tuned)**
    Saves performance metrics, predictions, and diagnostic plots to Outputs/Tables/ and Outputs/Visuals/.
---

## How to Download and Run

### Step 0 — Requirements

Install **R** and **RStudio** (recommended).
This project was run using R (version may vary).

### Step 1 — Get the repository

Clone or download this repository to your computer.

### Step 2 — Ensure the dataset is in the correct place

Ensure the original ONS Excel workbook is located at:

`Dataset/ONS_Business_Demography/ons_original.xlsx`

(If you rename the file or move it, update the `file_path` inside the scripts.)

### Step 3 — Install required R packages

Run the following in R once:

```r
install.packages(c(
  "tidyverse", "readxl", "readr", "dplyr", "tidyr", "stringr",
  "scales", "corrplot", "tidymodels", "ranger", "rsample", "purrr"
))
```


### Step 4 — Run scripts in order

Run scripts in this order:

1. **Data extraction scripts** (create cleaned CSV files from the Excel workbook)

   * `enterprise_births_industry.R`
   * `enterprise_deaths_industry.R`
   * `active_enterprises_industry.R`
   * `active_enterprises_10_employer_industry.R`
   * `high_growth_enterprises_industry.R`
   * `enterprise_survival_industry.R`

2. **Build the master panel**

   * `merge_industry_characteristics.R`

3. **EDA for RQ1**

   * `RQ1_EDA.R`
     Outputs saved to: `Outputs/Visuals/`

4. **Modelling for RQ2**

   * `RQ2_regression_model.R`
     Outputs saved to:
     `Outputs/Tables/` (metrics + predictions) and `Outputs/Visuals/` (plots)

---

## Outputs 

After running all scripts, you should see:

* `Dataset/Final_Master_Datasets/master_panel_industry_characteristics_2019_2023.csv`
* Multiple figures in `Outputs/Visuals/` (EDA + modelling diagnostics)
* Model results in `Outputs/Tables/`, including:

  * `RQ2_timeCV_metrics_with_baseline_rmse_r2_mae.csv`
  * `RQ2_timeCV_predictions_log_births_all_models.csv`
  * Random forest tuning tables and summary CSVs

---

## Notes on Reproducibility

* A fixed random seed is used in modelling (`set.seed(123)`).
* Cross-validation is **time-aware** (train years precede test years) to avoid using future information.
* The dataset is short (2019–2023), so results should be interpreted as **indicative**, not definitive.

