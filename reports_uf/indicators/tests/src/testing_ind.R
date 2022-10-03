#   # Introduction ------------------------------------------------------------------------------
# 
# Sensitivity and specificity test for use of force reports level 4 sample
#
#
# Author:  LMN 
# Date:  27-09-2022
# Organization: Kilometro Cero 

# Set up -----------------------------------------------------------------------

#Load packages
if(!require(pacman))install.packages("pacman")
p_load(dplyr, here, readr, lubridate, ggplot2, googlesheets4)


files <- list(input = ("1EVjtrWiIoyVWVb0a0El0qa64n8f0JLy0S_4Aw7ffZFs"), #Tengo que cambiarlo 
              output = here::here("reports_uf/indicators/test/output/indicators_tests.xlsx"))


# Import data -------------------------------------------------------------------
df_orig <- read_sheet(files$input,
                      sheet = "Incident level (Manual extraction)",
                      skip = 1) # Skip first row


# 

df <- df_orig %>% 
  mutate(across(where(is.character), ~na_if(., "N/A"))) %>% 
  select(33:54) %>% 
  rename( race_test = 1,
          race_result = 2,
          gender_test = 3,
          gender_result = 4,
          dviolence_test = 5,
          dviolence_result = 6,
          class_test = 7,
          class_result = 8,
          death_test = 9,
          death_result = 10,
          highcrim_test = 11,
          highcrim_result = 12,
          just_test = 13,
          just_result = 14,
          animal_test = 15,
          animal_result = 16,
          crisis_test = 17,
          crisis_result = 18,
          injury_test = 19,
          injury_result = 20,
          uf_34_test = 21, 
          uf_34_result = 22) 


# Grouping 
function(df)

# Race
race <- df %>% 
  group_by(race_test,race_result) %>% 
  count() %>% mutate(test_result = case_when(
      race_test == "1" & race_result == "1"  ~ "True Positive",
      race_test == "0" & race_result == "0" ~ "True Negative",
      race_test == "1" & race_result == "0" ~ "False Positive",
      race_test == "0" & race_result == "1" ~ "False Negative",
      TRUE ~ "none")) 

race <- race[1:4,]


# Gender
gender <- df %>% 
  select(starts_with("gender")) %>% 
  group_by_at(1:2) %>% 
  count() %>% mutate(test_result = case_when(
    test == "1" & df[2] == "1"  ~ "True Positive",
    df[1] == "0" & df[2] == "0" ~ "True Negative",
    df[1] == "1" & df[2]  == "0" ~ "False Positive",
    df[1] == "0" & df[2] == "1" ~ "False Negative",
    TRUE ~ "none")) 


# Domestic violence
dviolence <- df %>% 
  group_by(dviolence_test,dviolence_result) %>% 
  count() %>% mutate(test_result = case_when(
    dviolence_test == "1" & dviolence_result == "1"  ~ "True Positive",
    dviolence_test == "0" & dviolence_result == "0" ~ "True Negative",
    dviolence_test == "1" & dviolence_result == "0" ~ "False Positive",
    dviolence_test == "0" & dviolence_result == "1" ~ "False Negative",
    TRUE ~ "none")) 


# Social class 

Sclass <- df %>% 
  group_by(class_test,class_result) %>% 
  count() %>% mutate(test_result = case_when(
    class_test == "1" & class_result == "1"  ~ "True Positive",
    class_test == "0" & class_result == "0" ~ "True Negative",
    class_test == "1" & class_result == "0" ~ "False Positive",
    class_test == "0" & class_result == "1" ~ "False Negative",
    TRUE ~ "none")) 


# Death 
death <- df %>% 
  group_by(death_test,death_result) %>% 
  count() %>% mutate(test_result = case_when(
    death_test == "1" & death_result == "1"  ~ "True Positive",
    death_test == "0" & death_result == "0" ~ "True Negative",
    death_test == "1" & death_result == "0" ~ "False Positive",
    death_test == "0" & death_result == "1" ~ "False Negative",
    TRUE ~ "none")) 

# Crime rate
highcrim <- df %>% 
  group_by(highcrim_test,highcrim_result) %>% 
  count() %>% mutate(test_result = case_when(
    highcrim_test == "1" & highcrim_result == "1"  ~ "True Positive",
    highcrim_test == "0" & highcrim_result == "0" ~ "True Negative",
    highcrim_test == "1" & highcrim_result == "0" ~ "False Positive",
    highcrim_test == "0" & highcrim_result == "1" ~ "False Negative",
    TRUE ~ "none")) 


# Justification
Justification <- df %>% 
  group_by(just_test,just_result) %>% 
  count() %>% mutate(test_result = case_when(
    just_test == "1" & just_result == "1"  ~ "True Positive",
    just_test == "0" & just_result == "0" ~ "True Negative",
    just_test == "1" & just_result == "0" ~ "False Positive",
    just_test == "0" & just_result == "1" ~ "False Negative",
    TRUE ~ "none")) 

# Animal
animal <- df %>% 
  group_by(animal_test,animal_result) %>% 
  count() %>% mutate(test_result = case_when(
    animal_test == "1" & animal_result == "1"  ~ "True Positive",
    animal_test == "0" & animal_result == "0" ~ "True Negative",
    animal_test == "1" & animal_result == "0" ~ "False Positive",
    animal_test == "0" & animal_result == "1" ~ "False Negative",
    TRUE ~ "none")) 

# Patient in crisis

patient_crisis <- df %>% 
  group_by(crisis_test,crisis_result) %>% 
  count() %>% mutate(test_result = case_when(
    crisis_test == "1" & crisis_result == "1"  ~ "True Positive",
    crisis_test == "0" & crisis_result == "0" ~ "True Negative",
    crisis_test == "1" & crisis_result == "0" ~ "False Positive",
    crisis_test == "0" & crisis_result == "1" ~ "False Negative",
    TRUE ~ "none")) 
  

# Injury 
Injury <- df %>% 
  group_by(injury_test,injury_result) %>% 
  count() %>% mutate(test_result = case_when(
    injury_test == "1" & injury_result == "1"  ~ "True Positive",
    injury_test == "0" & injury_result == "0" ~ "True Negative",
    injury_test == "1" & injury_result == "0" ~ "False Positive",
    injury_test == "0" & injury_result == "1" ~ "False Negative",
    TRUE ~ "none")) 

# Use of force level 3-4
uf_34_ <- df %>% 
  group_by(uf_34_test,uf_34_result) %>% 
  count() %>% mutate(uf_34_result = case_when(
    uf_34_test == "1" & uf_34_result == "1"  ~ "True Positive",
    uf_34_test == "0" & uf_34_result == "0" ~ "True Negative",
    uf_34_test == "1" & uf_34_result == "0" ~ "False Positive",
    uf_34_test == "0" & uf_34_result == "1" ~ "False Negative",
    TRUE ~ "none")) 