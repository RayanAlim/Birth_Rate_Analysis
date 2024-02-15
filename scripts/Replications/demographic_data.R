#### Preamble ####
# Purpose: Demographic data to show the average number of births by state 
# variables affect minimum wage 
# Author: Maria Mangru
# Date: 15th February, 2024
# Contact: maria.mangru@utoronto.ca


library(haven)
library(tidyverse)
library(tidyr)


# Importing the CSV file
numbirths_2001_2019 <- read_csv("../data/analysis_data/annual_policy/numbirths_2001_2019.csv")

# Calculate average births scaled down by 1000 for visual representation
average_births_by_state <- numbirths_2001_2019 %>%
  filter(year >= 2001, year <= 2019) %>%
  group_by(stname) %>%
  summarize(AverageBirths = mean(numbirth1544, na.rm = TRUE) / 1000) %>% # Scale down by a factor of 1000
  ungroup()

# Create the bar plot with scaled averages
ggplot(average_births_by_state, aes(x = reorder(stname, -AverageBirths), y = AverageBirths)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Scaled Average Number of Births by State (2001-2019)",
       x = "State",
       y = "Scaled Average Number of Births") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### Tests ####
library(testthat)

# Test data import and initial processing
test_that("Data is imported and filtered correctly", {
  expect_true(exists("numbirths_2001_2019"))
  
  # Ensure the dataset contains the expected columns
  expect_true(all(c("year", "stname", "numbirth1544") %in% names(numbirths_2001_2019)))
  
  # Check if the filtering by year is correct
  expect_true(all(numbirths_2001_2019$year >= 2001 & numbirths_2001_2019$year <= 2019))
})







