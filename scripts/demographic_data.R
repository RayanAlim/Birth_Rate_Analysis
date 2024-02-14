library(haven)
library(tidyverse)
library(tidyr)


# Importing the CSV files
numbirths_2001_2019 <- read_csv("../data/analysis_data/annual_policy/numbirths_2001_2019.csv")
births_educ_race_age6 <- read_csv("../data/analysis_data/decomp/births_educ_race_age6.csv")
numbirths_educ_2044 <- read_csv("../data/analysis_data/educ/numbirths_educ_2044.csv")
state_births_04_19 <- read_csv("../data/analysis_data/map/state_births_04_19.csv") 

# Display the first few rows of each data-frame to verify the imports
view(numbirths_2001_2019)
view(births_educ_race_age6)
view(numbirths_educ_2044)
view(state_births_04_19)

### Number of Births ### 
colnames(numbirths_2001_2019)

average_births_by_state <- numbirths_2001_2019 %>%
  filter(year >= 2001, year <= 2019) %>%
  group_by(stname) %>%
  summarize(AverageBirths = mean(numbirth1544, na.rm = TRUE)) %>%
  ungroup()
view(average_births_by_state)


ggplot(average_births_by_state, aes(y = reorder(stname, AverageBirths), x = AverageBirths)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Average Number of Births by State (2001-2019)",
       y = "State",
       x = "Average Number of Births") +
  theme(axis.text.y = element_text(angle = 0)) 
<<<<<<< HEAD

=======
>>>>>>> 9e43bbea0ebd995129f3e0d14bc9e8ec625b3856


