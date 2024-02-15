#### Preamble ####
# Purpose: Determine the effect of an abortion delay policy on birth rates
# variables affect minimum wage 
# Author: Maria Mangru
# Date: 15th February, 2024
# Contact: maria.mangru@utoronto.ca


# Load libraries
library(readxl)
library(dplyr)
library(readr)
library(haven)
library(ggplot2)
library(lmtest)
library(sandwich)

# Define file paths 
policy_data_path <- "../data/analysis_data/annual_policy/policyvars01_19.dta"
population_data_path <- "../data/UKCPR_National_Welfare_Data_Update_020623.xlsx"
births_data_path <- "../data/analysis_data/annual_policy/numbirths_2001_2019.csv"


# Load and prepare the abortion policy data
abortion_policy <- read_dta(policy_data_path) %>%
  filter(year >= 2001, year <= 2019) %>%
  select(year, stname, delay)

# Load and prepare the population data
population_data <- read_excel(population_data_path, sheet = "Data") %>%
  select(state_name = stname, year, Population) %>%
  filter(year >= 2001, year <= 2019)

# Load and prepare the number of births data
numbirths_data <- read_csv(births_data_path) %>%
  rename(births = numbirth1544) %>%
  filter(year >= 2001, year <= 2019)

# Merge the datasets
births_population_data <- numbirths_data %>%
  left_join(population_data, by = c("year", "stname")) %>%
  mutate(relative_births = (births / Population) * 1000)

combined_data <- abortion_policy %>%
  left_join(births_population_data, by = c("year", "stname"))

# Define the groups based on predefined 'states_no_delay' and 'states_with_delay'
states_no_delay <- c("AK", "AL", "CA", "CO", "CT", "DC", "DE", "FL", "HI", "IA", "IL", "MA", "MD", "ME", "MT", "NH", "NJ", "NM", "NV", "NY", "OR", "RI", "TN", "VT", "WA", "WY")
states_with_delay <- c("AR", "ID", "IN", "KS", "KY", "LA", "MI", "MO", "MS", "ND", "NE", "OH", "PA", "SC", "SD", "UT", "VA", "WI")

# Filter data for states with no delay and plot
combined_data_no_delay <- combined_data %>%
  filter(stname %in% states_no_delay)

ggplot(data = combined_data_no_delay, aes(x = year, y = relative_births, group = stname, color = stname)) +
  geom_line() +
  labs(title = "Relative Average Number of Births in States Without Delay (2001-2019)",
       x = "Year",
       y = "Relative Average Number of Births per 1000 People",
       color = "State") +
  theme_minimal()

# Filter data for states with delay and plot
combined_data_with_delay <- combined_data %>%
  filter(stname %in% states_with_delay)

ggplot(data = combined_data_with_delay, aes(x = year, y = relative_births, group = stname, color = stname)) +
  geom_line() +
  labs(title = "Relative Average Number of Births in States With Delay (2001-2019)",
       x = "Year",
       y = "Relative Average Number of Births per 1000 People",
       color = "State") +
  theme_minimal()

# States which had a change in policy and plot
policy_changes <- data.frame(
  stname = c("AZ", "GA", "MN", "NC", "OK", "TX", "WV"),
  year_implemented = c(2009, 2005, 2003, 2011, 2005, 2003, 2003)
)

transition_states_data <- combined_data %>%
  filter(stname %in% policy_changes$stname)

# Filter and plot for states which had a change in policy
transition_states_data <- combined_data %>%
  filter(stname %in% policy_changes$stname)


ggplot(data = transition_states_data, aes(x = year, y = relative_births, group = stname, color = stname)) +
  geom_line() +
  geom_vline(data = policy_changes, aes(xintercept = year_implemented, color = stname), linetype="dashed") +
  geom_text(data = policy_changes, aes(x = year_implemented, y = Inf, label = year_implemented), vjust = -0.5, color = "black") +
  scale_x_continuous(breaks = seq(2001, 2019, by = 2)) +
                       labs(title = "Transition to Policy Delay and Relative Average Number of Births (2001-2019)",
                            x = "Year",
                            y = "Relative Average Number of Births per 1000 People",
                            color = "State") +
                       theme_minimal() +
                       theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Difference-in-Difference Analysis for NC with a policy change in 2011 compared to a no-delay state
did_data <- combined_data %>%
  filter(stname %in% c("NC", "TN"), year >= 2001, year <= 2019) %>%
  mutate(treatment = if_else(stname == "NC", 1, 0),
         post_policy = if_else(year >= 2011, 1, 0),
         treatment_x_post = treatment * post_policy)

# Fit the DiD model
did_model <- lm(relative_births ~ treatment + post_policy + treatment_x_post, data = did_data)

# Summary of the DiD model and robust standard errors
summary_did <- summary(did_model)
robust_summary <- coeftest(did_model, vcov = vcovHC(did_model, type = "HC1"))

# Output the summaries
print(summary_did)
print(robust_summary)

# Plotting for the DiD analysis
ggplot(did_data, aes(x = year, y = relative_births, color = stname)) +
  geom_line() +
  geom_vline(xintercept = 2011, linetype="dashed", color = "red") + # Policy change year
  labs(title = "Relative Births in NC (Treatment) and TN (Comparison) Over Time",
       x = "Year", y = "Relative Births per 1000 People") +
  theme_minimal()
