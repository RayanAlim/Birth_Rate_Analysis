#### Preamble ####
# Purpose: Simulates some of our main data sets including birth rates per states, political affiliation per states and more.
# Author: Rayan Awad Alim, Maria Mangu, Isha Junega
# Date: 15 February 2024 
# Contact: rayan.alim@mail.utoronto.ca, Maria.mangru@mail.utoronto.ca, isha.junega@mail.utoronto.ca
# License: MIT
# Pre-requisites: N/A


#### Simulate Political Affiliation States data ####
set.seed(23) 

#State Names
states <- c("AK", "AL", "AR", "AS", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "GU", "HI",
            "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MP", 
            "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA",
            "PR", "RI", "SC", "SD", "TN", "TX", "UM", "UT", "VA", "VI", "VT", "WA", "WI", "WV", "WY")


# Simulate fraction of Senate that is Democrats ( between 0 and 1)
fraction_of_senate_democrats <- runif(length(states))

# Simulate birthrate per 1000 population between 5 and 20 (Typical range from literature)
birthrate <- runif(length(states), min = 5, max = 20)

#Craete dataframe of simulated dayta
sim_datset <- data.frame(
  State = states,
  Fraction_of_senate_democrats = fraction_of_senate_democrats,
  Birthrate = birthrate
)

head(sim_datset)

## SOME TESTS FOR SIMULATED DATA ABOVE:
# Test 1: Number of items matches the number of states
stopifnot(length(states) == nrow(sim_datset))

# Test fraction of senate Democrats is between 0 and 1
stopifnot(all(
  fraction_of_senate_democrats >= 0 &
    fraction_of_senate_democrats <= 1
))

# Test birthrate is between 5 and 20
stopifnot(all(birthrate >= 5 & birthrate <= 20))

# Test data types
stopifnot(is.character(sim_datset$State)) # or is.factor, depending on your needs
stopifnot(is.numeric(sim_datset$Fraction_of_senate_democrats))
stopifnot(is.numeric(sim_datset$Birthrate))
