library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# Table 5.2a, Table 5.2b, Table 5.2c, Table 5.2d, Table 5.2e
# Births and survival (1–5 years) by industry
# Read one sheet per year (2019–2023) and combine into one dataset
# Output: Wide dataset (one row per industry-year)
file_path <- "./Dataset/ONS_Business_Demography/ons_original.xlsx"

# Sheets for 2019–2023
# One sheet per year
sheets_to_read <- c("Table 5.2a", "Table 5.2b", "Table 5.2c", "Table 5.2d", "Table 5.2e")

# Map sheet name to year (fixed table structure)
extract_year <- function(sheet_name) {
  case_when(
    sheet_name == "Table 5.2a" ~ 2019,
    sheet_name == "Table 5.2b" ~ 2020,
    sheet_name == "Table 5.2c" ~ 2021,
    sheet_name == "Table 5.2d" ~ 2022,
    sheet_name == "Table 5.2e" ~ 2023,
    TRUE ~ NA_real_
  )
}


clean_sheet_52 <- function(sheet_name) {
  
  year <- extract_year(sheet_name)
  
  # Read sheet content (skip header rows; no column names in the raw table)
  df <- read_excel(
    file_path,
    sheet = sheet_name,
    skip = 4,
    col_names = FALSE
  ) %>% 
    filter(!if_all(everything(), is.na))
  
  # Set consistent column names (trim to actual number of columns read)
  names(df) <- c(
    "industry_raw",
    "births",
    "survival_of_1_year", "percent_of_1_year",
    "survival_of_2_year", "percent_of_2_year",
    "survival_of_3_year", "percent_of_3_year",
    "survival_of_4_year", "percent_of_4_year",
    "survival_of_5_year", "percent_of_5_year"
  )[1:ncol(df)]
  
  # Extract industry code and drop non-data header rows inside the sheet
  df <- df %>%
    mutate(
      industry_raw = str_trim(as.character(industry_raw)),
      industry_code = str_extract(industry_raw, "^[0-9]{2,4}"),
      industry_level = case_when(
        nchar(industry_code) == 2 ~ "division",
        nchar(industry_code) == 3 ~ "group",
        nchar(industry_code) == 4 ~ "class",
        TRUE ~ "header"
      )
    ) %>%
    filter(industry_level != "header")   # remove section headers
  
  
  # Convert numeric columns (birth counts, survival counts, and percentages)
  df <- df %>%
    mutate(across(
      c(births,
        survival_of_1_year, survival_of_2_year, survival_of_3_year,
        survival_of_4_year, survival_of_5_year,
        percent_of_1_year, percent_of_2_year, percent_of_3_year,
        percent_of_4_year, percent_of_5_year),
      ~ as.numeric(str_replace_all(.x, ",", ""))
    ))
  
  
  df$year <- year
  
  # Add year and keep a clean, merge-ready schema
  df %>%
    select(
      year,
      industry_raw, industry_code, industry_level,
      births,
      survival_of_1_year, percent_of_1_year,
      survival_of_2_year, percent_of_2_year,
      survival_of_3_year, percent_of_3_year,
      survival_of_4_year, percent_of_4_year,
      survival_of_5_year, percent_of_5_year
    )
}

# Combine all years into one dataset
industry_survival_all <- bind_rows(
  lapply(sheets_to_read, clean_sheet_52)
)

write_csv(
  industry_survival_all,
  "./Dataset/ONS_Business_Demography/Births_and_Survival_of_Enterprises_2019_2023_by_Industry.csv"
)

View(industry_survival_all)
