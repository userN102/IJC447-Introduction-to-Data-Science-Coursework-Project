library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

# Table 7.4: Active enterprises (10+ employees) by industry
# Convert ONS Excel table to tidy long format (industry x year)
# Output: CSV saved in Dataset/ONS_Business_Demography/

file_path <- "./Dataset/ONS_Business_Demography/ons_original.xlsx"

# Read the table body (skip top header rows that aren't data)
df <- read_excel(file_path, sheet = "Table 7.4", skip = 3)

# Remove empty rows and columns
df <- df %>%
  select(where(~ !all(is.na(.)))) %>%
  filter(!if_all(everything(), is.na))

# Clean column names (remove spaces, Excel often adds extra spaces)
names(df) <- str_trim(names(df))


# Keep original industry label and trim whitespace
df <- df %>%
  rename(industry_raw = 1) %>%
  mutate(industry_raw = str_trim(industry_raw))

# Extract the industry code (first 2–3 digits before ":")
df <- df %>%
  mutate(
    industry_code = str_extract(industry_raw, "^[0-9]{2,3}"),
    industry_level = ifelse(nchar(industry_code) == 2, "division", "group")
  )

# Identify year columns, Year columns are those that look like "2019", "2020", etc
year_cols <- names(df)[str_detect(names(df), "^[0-9]{4}")]


# Convert year columns to numeric (remove commas like "12,345")
df <- df %>%
  mutate(across(all_of(year_cols), ~ {
    .x %>%
      as.character() %>%
      str_replace_all(",", "") %>%
      as.numeric()
  }))

# Reshape to long format: one row per industry-year
df_long <- df %>%
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "year",
    values_to = "active_enterprises_10plus_employees"
  )


# Save tidy dataset for merging into the master panel
write.csv(
  df_long,
  "./Dataset/ONS_Business_Demography/Active_Enterprises_10+_Emp_2019_2024_by_Industry.csv",
  row.names = FALSE
)

head(df_long)
