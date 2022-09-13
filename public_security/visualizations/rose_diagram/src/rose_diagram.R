# Introduction ------------------------------------------------------------------------------
# 
# Cleaninga petal diagram to present the duration of curfuews across time
# since the start of covid19 pandemic.
#
#
# Autor:  LMN 
# Fecha:  28-10-2021
# Organizacion: Kilometro Cero 

# Set up ----------------------------------------------------------------------



#Load packages
if(!require(pacman))install.packages("pacman")
p_load(dplyr, readxl,here)


files <- list(input = ("public_security/visualizations/rose_diagram/input/toques_queda.xlsx"), #Tengo que cambiarlo 
              output = here::here("public_security/visualizations/rose_diagram/output/rose_diag.pdf"))
            

              


# Import data -------------------------------------------------------------------
df <- read_excel(files$input,
                 sheet = 2)


# Remove extra rows and columns 
df <- df %>% slice(1:23,) %>% select("Orden Ejecutiva", "Fecha_comienzo", 
                                     "Hora_inicio","Hora_final","Hora_duracion") %>% 
  mutate(Fecha_comienzo = as.Date(df$Fecha_comienzo, format = "%d/%m/%Y"))



# Visualization ----------------------------------------------------------------
pdf(files$output)

mosaic <- treemap(df,
                  index= c("mecanismo","label"),
                  vSize="RIP",
                  vColor="color",
                  algorithm = "pivotSize",
                  align.labels=list(c("left", "bottom"), c("center", "center")),
                  fontsize.labels = c(14,14),
                  fontfamily.labels = "sans",
                  type="color",
                  border.lwds = 1,
                  fontcolor.labels = "black",
                  fontface.labels = "bold",
                  overlap.labels = 1)

dev.off() 


