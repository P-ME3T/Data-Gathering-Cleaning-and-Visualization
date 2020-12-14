# Importing Libraries
library(readr)
library(dplyr)
library(tidyr)


# Importing Data set
setwd("/Project")
Raw_Data <- read.csv("data/coviddata.csv")

# Converting into a Data frame
covid_DS <- data.frame(Raw_Data)

# Total Missing Values in data set
sum(is.na(covid_DS))
summary(covid_DS)

# Handling Missing values
covid_DS <- covid_DS[covid_DS$location!="International",]   
covid_DS$continent[covid_DS$continent == ""] <- "OVERALL"

# Cases before the first appearance replaced with zero.
covid_DS$new_cases[is.na(covid_DS$new_cases)] <- 0
sum(is.na(covid_DS$new_cases))

covid_DS$new_deaths[is.na(covid_DS$new_deaths)] <- 0
sum(is.na(covid_DS$new_deaths))

covid_DS$new_cases_smoothed[is.na(covid_DS$new_cases_smoothed)] <- 0
covid_DS$new_deaths_smoothed[is.na(covid_DS$new_deaths_smoothed)] <- 0

# NA values of total cases and total deaths are replaced with cumulative new cases.
test = covid_DS %>% 
  select(location, new_cases) %>% 
  group_by(location) %>%
  mutate(cs = cumsum(new_cases))
covid_DS$total_cases <- test$cs

testdeaths = covid_DS %>% 
  select(location, new_deaths) %>% 
  group_by(location) %>% 
  mutate(cs = cumsum(new_deaths))
covid_DS$total_deaths <- testdeaths$cs

# NA values of new_cases_per_million, total_cases_per_million, new_deaths_per_million and total_deaths_per_million are calculated using appropriate formulas and replaced.

new_cases_perm = covid_DS %>%
  select(location, new_cases, population) %>% 
  group_by(location) %>% 
  mutate(cs = new_cases*1000000/population)
covid_DS$new_cases_per_million <- new_cases_perm$cs

total_cases_perm = covid_DS %>%
  select(location, total_cases, population) %>% 
  group_by(location) %>% 
  mutate(cs = total_cases*1000000/population)
covid_DS$total_cases_per_million <- total_cases_perm$cs

new_deaths_perm = covid_DS %>%
  select(location, new_deaths, population) %>% 
  group_by(location) %>%  
  mutate(cs = new_deaths*1000000/population)
covid_DS$new_deaths_per_million <- new_deaths_perm$cs

total_deaths_perm = covid_DS %>%
  select(location, total_deaths, population) %>% 
  group_by(location) %>% 
  mutate(cs = total_deaths*1000000/population)
covid_DS$total_deaths_per_million <- total_deaths_perm$cs

# Finding the least relevant rows for visualization containing NA values.
sum(is.na(covid_DS$stringency_index) | is.na(covid_DS$population_density) |
      is.na(covid_DS$median_age) | is.na(covid_DS$aged_65_older) |
      is.na(covid_DS$aged_70_older) | is.na(covid_DS$gdp_per_capita) |
      is.na(covid_DS$cardiovasc_death_rate) | is.na(covid_DS$diabetes_prevalence) | 
      is.na(covid_DS$life_expectancy) | is.na(covid_DS$human_development_index))

# Removing rows containing NA values in specified rows whose values are difficult to obtain(even on internet).
covid_DS<-covid_DS %>%
  filter(!is.na(covid_DS$stringency_index) & !is.na(covid_DS$median_age) & 
                                !is.na(covid_DS$aged_65_older) & !is.na(covid_DS$aged_70_older) &
                                !is.na(covid_DS$cardiovasc_death_rate) & !is.na(covid_DS$diabetes_prevalence) & 
                                !is.na(covid_DS$life_expectancy) & !is.na(covid_DS$human_development_index))

sum(is.na(covid_DS$stringency_index) | is.na(covid_DS$median_age) |
      is.na(covid_DS$aged_65_older) | is.na(covid_DS$aged_70_older) |
      is.na(covid_DS$cardiovasc_death_rate) | is.na(covid_DS$diabetes_prevalence) | 
      is.na(covid_DS$life_expectancy) | is.na(covid_DS$human_development_index))

# Population density replaces NA with original value
covid_DS <-  covid_DS %>%
  mutate(population_density=replace(population_density,location=="South Sudan",18))

# GDP per capital NA values replaced with suitable values.
covid_DS <-  covid_DS %>%
  mutate(gdp_per_capita=replace(gdp_per_capita,location=="Cuba",8821.82))

# Dropping unnecessary columns not required for analysis and Visualizations.
covid_DS <- covid_DS %>%
  select(!(female_smokers:hospital_beds_per_thousand))

covid_DS <- covid_DS %>%
  select(!(reproduction_rate:weekly_hosp_admissions_per_million))

covid_DS <- covid_DS %>%
  select(!(total_tests:tests_units))

covid_DS <- covid_DS %>%
  select(!(extreme_poverty))

covid_DS <- covid_DS %>%
  select(!(new_cases_smoothed_per_million))

covid_DS <- covid_DS %>%
  select(!(new_deaths_smoothed_per_million))


# Missing Values Check
sum(is.na(covid_DS))

# Exporting Data Frame into excel and csv
library(writexl)
write_xlsx(covid_DS,"data/cleanedData.xlsx")
write.csv(covid_DS,"data/cleanedData.csv")


