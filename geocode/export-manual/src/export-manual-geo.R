# Authors:     LM
# Maintainers: LM
# Date: 6-Mar-24
# =========================================

# Exporting cases of interest for geocoding 
# source = Death Registry database


# --- libs --- 
if(!require(pacman))install.packages("pacman")
p_load(dplyr, 
       here,
       data.table,
       read.xl,
       stringr)


args <- list(input = here("geocode/import/output/fatal-victims.csv"),
             output = here("geocode/manual/output/geocode-fatal-victims.xlsx"))


# --- Import data --- 
fatal_victims <- fread(args$input)



# --- Cleaning --- 

## SEPARATE THEM IN DIFFERENT YEAR EACH SHEET
out <- fatal_victims %>% filter(AñoRIP != 2023) %>% 
  select(`ID#`,Nom,FechaRIP,AñoRIP,RZIP,MuniResi,RDir) %>% 
  mutate(longitude = NA,
         latitute = NA) %>% 
  mutate_all(~tolower(.)) %>%
  mutate_all(~str_squish(.))




# --- Output --- 
write.xlsx(out, 
           file = args$output, 
           sheetNames = geocode_manual)


## DONE
