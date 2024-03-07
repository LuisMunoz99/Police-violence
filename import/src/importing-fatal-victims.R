# Authors:     LM
# Maintainers: LM
# Date: 6-Mar-24
# ===========================================

# -- libs ---
if(!require(pacman))install.packages("pacman")
p_load(here,
       dplyr,
       readxl)

# args {{{
args <- list(
  input = here("import/input/Interno - Banco de datos de victimas fatales de la Policia de Puerto Rico; a partir de 2014.xlsx"),
  output = here("import/output/fatal-victims.csv")
)

# -- Import ---

all_sheets <- excel_sheets(args$input)
print(all_sheets)

# Empty list
fatal_victims <- list()

# For sheet_names
for (ind in all_sheets) {
  fatal_victims[[ind]] <- read_excel(args$input, sheet = ind)
}

# All sheets were imported in case we need to use another sheet in the future
# for now we will use "Geocodificacion" and "2. Casos de víctimas fatales" and
# "4. Mecanismo de muerte"


# -- Cleaning ---

## Sheet = Geocodificacion
geo <- fatal_victims$Geocodificacion

head(geo)



# Sheet = 

# -- Output ---

             