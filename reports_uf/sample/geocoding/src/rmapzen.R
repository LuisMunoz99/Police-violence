# 
# Geocoding "lugar" variable from sample of level 4 use of force reports. 
# 1. Expand and parse adresses using libpostal.
# 2. Improve parsing
# 3. Use rmapzen: Openworld API --> search, 
#     A. Search
#     B. String similarity - choose
#     C. mz_geocode - lat and long
# 4. Use censusxy 
#     A. adapt addresses to USPS format
#     B. cxy_geocode

#
# Author:  LMN 
# Date:  09-11-2022
# Organization: Kilometro Cero 

# Set up

#Load packages
if(!require(pacman))install.packages("pacman")
p_load(dplyr, here, readr, poster, stringr, rmapzen)

date <- as.character(Sys.Date())

files <- list(input = here::here("reports_uf/sample/descriptive_stats/input/book2.csv"), #Tengo que cambiarlo 
              output = here::here(paste0("reports_uf/sample/geocoding/output/","geo_rmapzen",date, ".csv")))
              

geosearch <- safely(slowly(mz_search, rate = rate_delay(pause = .15)))
geocode <- safely(slowly(mz_geocode, rate = rate_delay(.15)))


# Import data
df_orig <- read_csv(files$input) # Skip first row

df <- df_orig 

lugar <- df %>% 
  slice(1:80) %>% 
  mutate(across(where(is.character), ~ na_if(., "N/A"))) %>% 
  select(1,11) # Incident ID & lugar

# Expanding address
std_lugar <- lugar %>% mutate(lib_lugar = normalise_addr(lugar))
std_lugar$lib_lugar <- str_replace(std_lugar$lib_lugar, "carrera", "carretera")


# parsing address
parse_lugar <- parse_addr(std_lugar$lib_lugar)


#Adding information 
head(parse_lugar)
parse_lugar <- parse_lugar %>% mutate(state = "PR")


# Tarak
all <- std_lugar$lib_lugar
test <- map(samp, geosearch, size = 5)


# Rmapzen 
mz_set_search_host_geocode.earth("ge-bd28d9908a18bb29")



all_searches_std_lugar <- list(map(all, mz_search, size = 5))


