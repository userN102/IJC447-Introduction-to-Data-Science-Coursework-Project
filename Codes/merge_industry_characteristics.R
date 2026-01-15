library(dplyr)
library(readr)

# Build master panel (division level) by merging cleaned ONS tables
# Years: 2019–2023
# Output: master_panel_industry_characteristics_2019_2023.csv

# Base table: births (keeps industry label + name for reporting)
births <- read_csv("./Dataset/ONS_Business_Demography/Births_Of_New_Enterprises_2019_2024_by_Industry.csv") %>%
  filter(industry_level == "division", year >= 2019,
         year <= 2023) %>%
  mutate(
    # Industry name = text after "NN: "
    industry_name = stringr::str_trim(
      stringr::str_remove(industry_raw, "^[0-9]{2}\\s*:\\s*")
    )
  ) %>%
  select(industry_raw, industry_code,industry_name, year, births_of_new_enterprises)


# Active enterprises (all sizes)
active_all <- read_csv("./Dataset/ONS_Business_Demography/Active_Enterprises_2019_2024_by_Industry.csv") %>%
  filter(industry_level == "division", year >= 2019,
         year <= 2023) %>%
  select(industry_code, year, active_enterprises)


# Active enterprises (10+ employees)
active_10 <- read_csv("./Dataset/ONS_Business_Demography/Active_Enterprises_10+_Emp_2019_2024_by_Industry.csv") %>%
  filter(industry_level == "division", year >= 2019,
         year <= 2023) %>%
  select(industry_code, year, active_enterprises_10plus_employees)


# Deaths of new enterprises
deaths <- read_csv("./Dataset/ONS_Business_Demography/Deaths_Of_New_Enterprises_2019_2024_by_Industry.csv") %>%
  filter(industry_level == "division", year >= 2019,
         year <= 2023) %>%
  select(industry_code, year, deaths_of_new_enterprises)


# High-growth enterprises
high_growth <- read_csv("./Dataset/ONS_Business_Demography/High_Growth_Enterprises_2019_2024_by_Industry.csv") %>%
  filter(industry_level == "division", year >= 2019,
         year <= 2023) %>%
  select(industry_code, year, high_growth_enterprises)


# Survival (rates + counts) for births cohorts (2019–2023)
survival <- read_csv("./Dataset/ONS_Business_Demography/Births_and_Survival_of_Enterprises_2019_2023_by_Industry.csv") %>%
  filter(industry_level == "division") %>%
  select(
    industry_code,
    year,
    percent_of_1_year,
    percent_of_2_year,
    percent_of_3_year,
    percent_of_4_year,
    percent_of_5_year,
    survival_of_1_year,
    survival_of_2_year,
    survival_of_3_year,
    survival_of_4_year,
    survival_of_5_year
  )


# Merge all indicators onto the births base table
master_panel <- births %>%
  left_join(active_all,  by = c("industry_code", "year")) %>%
  left_join(active_10,   by = c("industry_code", "year")) %>%
  left_join(deaths,      by = c("industry_code", "year")) %>%
  left_join(survival,    by = c("industry_code", "year")) %>%
  left_join(high_growth, by = c("industry_code", "year"))


# Save final master panel for EDA and modelling
write_csv(
  master_panel,
  "./Dataset/Final_Master_Datasets/master_panel_industry_characteristics_2019_2023.csv"
)

