library(readxl)
library(dplyr)
library(ggplot2)

# Load Data 
welfare_data <- read_excel("../data/UKCPR_National_Welfare_Data_Update_020623.xlsx", sheet = "Data")
View(welfare_data)

colnames(welfare_data)

# Retrieving data for state minimum wage for 2001 - 2019
filtered_minwage <- welfare_data %>%
  select(state_name, year, `State Minimum Wage`) %>%
  filter(year >= 2001, year <= 2019)

view(filtered_minwage)


##### State Minimum Wage #####
# Filter data for 2001 and 2019
minwage_2001 <- welfare_data %>%
  filter(year == 2001) %>%
  select(state_name, year, `State Minimum Wage`)

minwage_2019 <- welfare_data %>%
  filter(year == 2019) %>%
  select(state_name, year, `State Minimum Wage`)

# Rename the `State Minimum Wage` columns to reflect the year
minwage_2001 <- minwage_2001 %>%
  rename(MinWage2001 = `State Minimum Wage`)

minwage_2019 <- minwage_2019 %>%
  rename(MinWage2019 = `State Minimum Wage`)

# Merge the 2001 and 2019 data by state_name
minwage_change <- left_join(minwage_2001, minwage_2019, by = "state_name")

# Calculate the change in minimum wage from 2001 to 2019
minwage_change <- minwage_change %>%
  mutate(ChangeInMinWage = MinWage2019 - MinWage2001) %>%
  select(state_name, MinWage2001, MinWage2019, ChangeInMinWage)

# View the table
View(minwage_change)
write.csv(minwage_change, "minwage_change_by_state.csv", row.names = FALSE)

##### Federal minimum wage #####
federal_min_wage_data <- welfare_data %>%
  select(year, `Federal Minimum Wage`) %>%
  filter(year >= 2001, year <= 2019)

# Create the line graph
ggplot(federal_min_wage_data, aes(x = year, y = `Federal Minimum Wage`)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Federal Minimum Wage from 2001 to 2019",
       x = "Year",
       y = "Federal Minimum Wage") +
  theme(legend.position = "none")


