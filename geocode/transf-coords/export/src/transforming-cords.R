#code to transform sf objects in progress NOT READY 

# libs
if(!require(pacman))install.packages("pacman")
p_load(data.table,
       sf,
       tidycensus,
       dplyr, 
       tidyr)


# Useful function 


# args {{{
args <- list(input = here("geocode/transf-cords/import/input/_____") NOT DONE
             output =  here("geocode/transf-cords/import/output/geocoded-coords.csv")
             # }}}
             
             # Deaths data import -----------------------------------------------------
             
             # Importing Banco de victimas fatales interno 
             df_orig <- read_sheet(args$input, sheet = "Geocodificacion")
             
             
             df <- df_orig %>% select(Nom,FechaRIP,latitud,longitud,`Census tract`) %>% 
               rename(longitude = longitud,
                      latitude = latitud) %>% filter(!is.na(longitude))
             
             
             
             # Converting the coordinates into simple features objects
             df_coords <- df %>%
               st_as_sf(coords = c("longitude", "latitude"),
                        crs = 4326) %>% st_transform(6440)
             
             
             # Importing geometry census data
             
             PR_tract <- get_acs(
               geography = "tract",
               variables = c("B01003_001"),
               state = "PR",
               year = 2021,
               geometry = TRUE
             ) %>%
               st_transform(6440)
             
             
             # Merging data 
             census_tracts_RIP <- st_join(
               df_coords,
               PR_tract
             ) 
             
             ## Verificar si esto resuelve duplicados 
             census_tracts_RIP <- census_tracts_RIP %>%
               group_by(GEOID,NAME) %>%
               summarise(NomList = list(Nom), Count = n()) %>% 
               mutate(NomList = as.character(NomList))
             
             
             
             
             # Output
             write.csv(census_tracts_RIP, "input/census_tracts_RIP.csv", row.names = FALSE)
             
             
             
             
             
             
             
             