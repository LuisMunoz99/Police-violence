#   # Introduction ------------------------------------------------------------------------------
# 
# Descriptive stats on sample of reports 
#
#
# Author:  LMN 
# Date:  27-09-2022
# Organization: Kilometro Cero 

# Set up -----------------------------------------------------------------------

#Load packages
if(!require(pacman))install.packages("pacman")
p_load(dplyr, here, readr, table1)


files <- list(input = here::here("reports_uf/sample/descriptive_stats/input/book2.csv"), #Tengo que cambiarlo 
              output = here::here("reports_uf/sample/output/sample_stats.xlsx"))


# Import data -------------------------------------------------------------------
df_orig <- read_csv(files$input) # Skip first row

df <- df_orig %>% slice(1:80) %>% 
  mutate(across(where(is.character), ~na_if(., "N/A"))) 


# Tarak data
df_tarak <- df %>% 
  select(1:14) %>% 
  mutate(edad = as.numeric(edad))
  

table1(~ under_investigation + edad + genero + muni + nvl_fuerza, data = df_tarak)
   

# Km0 checkbox
df_checkbox <- df %>% 
  select(19:24) %>% 
  rename(factores_externos = 1,
          determinacion_supervisor = 2,
          herida_vic = 3,
          herida_ppr = 4,
          tecnicas_empleadas = 5,
          muerte = 6) 
 
         
table1(~ factores_externos + determinacion_supervisor 
       + herida_vic + herida_ppr + muerte, data = df_checkbox)





# Km0 narrative (manual extraction)
df_narrative <- df %>% 
  select(25:32) %>% 
  rename(lugar = 1,
          hora = 2,
          menor_edad = 3,
          causo_intervencion = 4,
          causo_disparo = 5,
          consecuencia_disparo = 6,
          persecusion_veh = 7,
          resultado_final = 8)

table1(~ menor_edad + causo_intervencion + causo_disparo + consecuencia_disparo +
         persecusion_veh + resultado_final,data = df_narrative)


# Indicators 
  
df_indic <- df %>% 
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

df_indic %>% group_by(race_test, race_result) %>% count()

table1(~ race_test + race_result, data = df_indic)
table1(~gender_test + gender_result, data = df_race)



