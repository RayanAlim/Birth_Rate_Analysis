#### Preamble ####
# Purpose: Determines the impact of minimum wage on number of births 
# Author: Maria Mangru
# Date: 15th February, 2024
# Contact: maria.mangru@utoronto.ca


# Load libraries
library(readxl)
library(readr)
library(dplyr)
library(ggplot2)
library(testthat)

# Load the data sets
welfare_data <- read_excel("../data/UKCPR_National_Welfare_Data_Update_020623.xlsx", sheet = "Data") %>%
  select(state_name, year, `State Minimum Wage`) %>%
  filter(year >= 2001 & year <= 2019)

numbirths_2001_2019 <- read_csv("../data/analysis_data/annual_policy/numbirths_2001_2019.csv") %>%
  select(year, stname, numbirth1544) %>%
  filter(year >= 2001 & year <= 2019) %>%
  rename(state_name = stname)

# Merge data sets on state_name and year
merged_data <- merge(welfare_data, numbirths_2001_2019, by = c("state_name", "year"))

# Calculate average minimum wage and average number of births per year 
average_data <- merged_data %>%
  group_by(year) %>%
  summarise(Average_Minimum_Wage = mean(`State Minimum Wage`, na.rm = TRUE),
            Average_Births = mean(numbirth1544, na.rm = TRUE))


# Find maximum values for scaling 
max_births <- max(average_data$Average_Births, na.rm = TRUE)
max_wage <- max(average_data$Average_Minimum_Wage, na.rm = TRUE)
scaling_factor <- max_wage / max_births


# Plot the graph with adjusted scales
ggplot(average_data) +
  geom_line(aes(x = year, y = Average_Minimum_Wage), colour = "blue", size = 1) +
  geom_line(aes(x = year, y = Average_Births * scaling_factor), colour = "red", size = 1) +
  scale_y_continuous(
    "Minimum Wage",
    sec.axis = sec_axis(~ . / scaling_factor, name = "Number of Births")
  ) +
  theme_minimal() +
  labs(title = "Comparison of Average Minimum Wage and Birth Rates (2001-2009)",
       x = "Year")

#### Tests ####

# Test data loading and transformations
test_that("Data is loaded correctly", {
  expect_true(exists("welfare_data"))
  expect_true(exists("numbirths_2001_2019"))
  
  # Check if the required columns are present
  expect_true(all(c("state_name", "year", "State Minimum Wage") %in% names(welfare_data)))
  expect_true(all(c("year", "state_name", "numbirth1544") %in% names(numbirths_2001_2019)))
  
  # Ensure filtering is correct
  expect_true(all(welfare_data$year >= 2001 & welfare_data$year <= 2019))
  expect_true(all(numbirths_2001_2019$year >= 2001 & numbirths_2001_2019$year <= 2019))
})

# Test calculations are correct
test_that("Calculations are correct", {
  expect_true(exists("average_data"))
  expect_true(exists("max_births"))
  expect_true(exists("max_wage"))
  expect_true(exists("scaling_factor"))
  
  # Check for correct summarization
  expect_true(all(c("Average_Minimum_Wage", "Average_Births") %in% names(average_data)))
  
  # Validate scaling factor calculation
  expect_equal(scaling_factor, max_wage / max_births)
})













