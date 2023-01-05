# 
# Geocoding "place" variable from sample of level 4 use of force reports. 
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

p_load(dplyr,
       here, 
       readr, 
       poster, 
       stringr,
       censusxy)

date <- as.character(Sys.Date())

files <- list(input = here::here("reports_uf/sample/descriptive_stats/input/book2.csv"), #Tengo que cambiarlo 
              output = here::here(paste0("reports_uf/sample/geocoding/output/","geo_censusxy",date, ".csv")))


# Import data
df_orig <- read_csv(files$input)
df <- df_orig 


# Functions
clstr <- function(str) str_squish(str_to_upper(str)) %>%
  stri_trans_general("Latin-ASCII")

add_muni <- function(loc, muni) {
  muni <- clstr(muni)
  loc <- clstr(loc)
  noupdate <- str_detect(loc, muni)
  if_else(noupdate, loc, paste(loc, muni))
}

loc <- df %>% 
  slice(1:80) %>% 
  mutate(across(where(is.character), ~ na_if(., "N/A"))) %>%
  filter(!is.na(lugar)) %>%
  mutate(muni = replace_na(muni, "")) %>%
  transmute(lugar, muni = str_replace_all(muni, "\\-", " ")) %>%
  mutate(location = add_muni(lugar, muni)) %>%
  mutate(location = paste(location, "PUERTO RICO"))



# Expanding address

std_loc <- loc %>% mutate(lib_loc = normalise_addr(location)) %>% 
  mutate(lib_loc = str_replace(lib_loc, "carrera", "carretera"))


# parsing address df
parse_loc <- parse_addr(std_loc$lib_loc)
parse_loc <- parse_loc %>% mutate(state = "PR")

# Geocoding censusxy 
# Problema buscar las dirreciones dentro de geografias solo se puede para casos individuales
# Tigris y sf son los recomendados 


df_sf <- cxy_geocode(parse_loc,
                     street = "road",
                     city = "city",
                     state = "state",
                     class = "dataframe",
                     output = "full")

cx


# Coordinates to census tract
cxy_geography(lon = -66.70808856931151, lat = 18.266090882838363)


mapview::mapview(df_sf)

x <- cxy_benchmarks()


# only 2 outputs 


