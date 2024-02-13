library(haven)
library(tidyverse)
library(tidyr)


# Importing the CSV files
numbirths_2001_2019 <- read_csv("annual_policy/numbirths_2001_2019.csv")
births_educ_race_age6 <- read_csv("decomp/births_educ_race_age6.csv")
numbirths_educ_2044 <- read_csv("educ/numbirths_educ_2044.csv")
state_births_04_19 <- read_csv("map/state_births_04_19.csv") 

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





### Number of Births for Education - REMOVE ### 
filtered_numbirths_edu <- numbirths_educ_2044 %>%
  filter(year >= 2001, year <= 2009)

long_data <- filtered_numbirths_edu %>%
  pivot_longer(cols = c("numbirth_lesshs", "numbirth_hsgrad", "numbirth_somecoll", "numbirth_colgrad", "numbirthalleduc", "numbirth_witheduc"),
               names_to = "education_level",
               values_to = "number_of_births")

ggplot(long_data, aes(x = year, y = number_of_births, color = education_level, group = education_level)) +
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(title = "Number of Births by Educational Attainment (2001-2009)",
       x = "Year",
       y = "Number of Births",
       color = "Education Level") +
  scale_x_continuous(breaks = 2001:2009) 

