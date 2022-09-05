# Introduction -----------------------------------------------------------------
#  Preparing a treemap/square chart to visualize the distribution of deaths from 
#  lethal police use of by mechanism used. 
#
#
#  fecha: 2/22/22


# Set up -----------------------------------------------------------------------

#Load  packages
if(!require(pacman))install.packages("pacman")
p_load(dplyr,treemap,here,data.table,d3treeR)


files <- list(input = here::here("visualizations/mosaic/input/mosaico_LPM_R.csv"),
              output = here::here("visualizations/mosaic/output/mosaic.pdf"))

# Import data -------------------------------------------------------------------

df_orig <- fread(files$input)
df <- df_orig %>% rename(mecanismo = `Mecanismo de muerte`) %>% arrange(desc(RIP)) 

# Color palette
df$color <- c("#FFAF3A","#FEBC59","#FADDCA","#F8CBAD","#FAB183","#9464B8","#D4834D","#D4834D") 

# Labels (Hardcoded)
df$label <- c("36 (51%)","10 (14%)","8 (11%)","6 (8%)","4 (6%)","3 (4%)","2 (3%)","2 (3%)")


# Data cleaning ----------------------------------------------------------------

df$mecanismo <- recode(df$mecanismo, 
                       "Disparo arma de reglamento, Policia Estatal" = "Disparo arma de reglamento, Policía Estatal",
                       "Disparo arma de reglamento, Policia Municipal" = "Disparo arma de reglamento, Policía Municipal  ", 
                       "Suicidio de policia" = "Suicidio de policía",
                       "Persecusion vehicular" = "Persecusión vehicular",
                       "Feminicidio intimo" = "Feminicidio íntimo")




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

