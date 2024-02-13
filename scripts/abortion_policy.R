library(readxl)
library(dplyr)
library(readr)
library(haven)

# Determine which states had a delay or no delay policy 
abortion_policy <- read_dta("../data/analysis_data/annual_policy/policyvars01_19.dta") %>%
  filter(year >= 2001, year <= 2019) %>%
  select(delay, stname, year)

# Load the population data from the Excel file
population_data <- read_excel("../data/UKCPR_National_Welfare_Data_Update_020623.xlsx", sheet = "Data") %>%
  select(state_name, year, Population) %>%
  filter(year >= 2001, year <= 2019)

numbirths_data <- read_csv("../data/analysis_data/annual_policy/numbirths_2001_2019.csv") %>%
  rename(births = numbirth1544) %>%
  filter(year >= 2001, year <= 2019)

# Ensure both data sets have the 'stname' column for joining
population_data <- population_data %>%
  rename(stname = state_name)

# Merge the number of births data with the population data
births_population_data <- numbirths_data %>%
  left_join(population_data, by = c("year", "stname"))

# Calculate the relative average number of births per 1000 people
births_population_data <- births_population_data %>%
  mutate(relative_births = (births / Population) * 1000)

# Load the abortion policy data
abortion_policy <- read_dta("../data/analysis_data/annual_policy/policyvars01_19.dta") %>%
  select(year, stname, delay)

# Merge the relative births data with the abortion policy data
combined_data <- abortion_policy %>%
  left_join(births_population_data, by = c("year", "stname"))

# Define the groups based on predefined 'states_no_delay' and 'states_with_delay'
states_no_delay <- c("AK", "AL", "CA", "CO", "CT", "DC", "DE", "FL", "HI", "IA", "IL", "MA", "MD", "ME", "MT", "NH", "NJ", "NM", "NV", "NY", "OR", "RI", "TN", "VT", "WA", "WY")
states_with_delay <- c("AR", "ID", "IN", "KS", "KY", "LA", "MI", "MO", "MS", "ND", "NE", "OH", "PA", "SC", "SD", "UT", "VA", "WI")

# Filter the combined_data for states with no delay
combined_data_no_delay <- combined_data %>%
  filter(stname %in% states_no_delay)

# Filter the combined_data for states with delay
combined_data_with_delay <- combined_data %>%
  filter(stname %in% states_with_delay)

#### Plot for states with no policy delay ####
ggplot(data = combined_data_no_delay) +
  geom_line(aes(x = year, y = relative_births, group = stname, color = stname)) +
  labs(title = "Relative Average Number of Births in States Without Delay (2001-2019)",
       x = "Year",
       y = "Relative Average Number of Births per 1000 People",
       color = "State") +
  theme_minimal()

#### Plot for states with policy delay ####
ggplot(data = combined_data_with_delay) +
  geom_line(aes(x = year, y = relative_births, group = stname, color = stname)) +
  labs(title = "Relative Average Number of Births in States With Delay (2001-2019)",
       x = "Year",
       y = "Relative Average Number of Births per 1000 People",
       color = "State") +
  theme_minimal()


#### States which had a change in policy ####
# States and years of policy implementation
policy_changes <- data.frame(
  stname = c("AZ", "GA", "MN", "NC", "OK", "TX", "WV"),
  year_implemented = c(2009, 2005, 2003, 2011, 2005, 2003, 2003)
)

# Filter combined_data
transition_states_data <- combined_data %>%
  filter(stname %in% policy_changes$stname)

# Creating the graph
ggplot(data = transition_states_data) +
  geom_line(aes(x = year, y = relative_births, group = stname, color = stname)) +
  geom_vline(data = policy_changes, aes(xintercept = year_implemented, color = stname), linetype="dashed") +
  geom_text(data = policy_changes, aes(x = year_implemented, y = Inf, label = year_implemented), vjust = -0.5, color = "black") +
  scale_x_continuous(breaks = seq(2001, 2019, by = 2)) +
  labs(title = "Transition to Policy Delay and Relative Average Number of Births (2001-2019)",
       x = "Year",
       y = "Relative Average Number of Births per 1000 People",
       color = "State") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#### Selected comparison states ####
# Step 1: Select states for comparison
states_comparison <- combined_data %>%
  filter(stname %in% c("CA", "TX", "GA", "NC", "FL"))

# Step 2: Calculate descriptive statistics and visualize trends
# For California (no policy change) and Texas (policy change in 2003)
ca_tx_data <- states_comparison %>%
  filter(stname %in% c("CA", "TX"))

# Calculate summary statistics for CA and TX before and after 2003
ca_tx_summary <- ca_tx_data %>%
  group_by(stname) %>%
  summarise(
    mean_before = mean(relative_births[year < 2003], na.rm = TRUE),
    mean_after = mean(relative_births[year >= 2003], na.rm = TRUE)
  )

# For Georgia (policy change in 2005), North Carolina (policy change in 2011), and Florida (no policy change)
ga_nc_fl_data <- states_comparison %>%
  filter(stname %in% c("GA", "NC", "FL"))

# Calculate summary statistics for GA and NC before and after policy changes
ga_nc_fl_summary <- ga_nc_fl_data %>%
  group_by(stname) %>%
  summarise(
    mean_before = mean(relative_births[year < if_else(stname == "GA", 2005, if_else(stname == "NC", 2011, year))], na.rm = TRUE),
    mean_after = mean(relative_births[year >= if_else(stname == "GA", 2005, if_else(stname == "NC", 2011, year))], na.rm = TRUE)
  )

# Step 3: Visualize trends
ggplot(states_comparison, aes(x = year, y = relative_births, color = stname)) +
  geom_line() +
  geom_vline(xintercept = c(2003, 2005, 2011), linetype = "dashed", color = "black") +
  labs(title = "Relative Birth Rates in Selected States (2001-2019)", y = "Relative Births per 1000 People", x = "Year") +
  theme_minimal()



