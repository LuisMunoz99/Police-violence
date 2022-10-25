# Introduction ------------------------------------------------------------------------------
# 
# Creating moving average linear plot of new cases of covid between 15/03/2020 
# to 24/may/2021 
#
#
# Author:  LMN 
# Date:  03-10-2022
# Organization: Kilometro Cero 

# Set up ----------------------------------------------------------------------



#Load packages
if(!require(pacman))install.packages("pacman")
p_load(dplyr, here,lubridate, readr, ggplot2, googlesheets4, zoo, 
       showtext, stringr, plotly)

files <- list(input = ("1gwA6FlA_mOLlwJCHvBjZ6mnRTcSltUbaFKVy9DS7h44"), #
              output = here::here("public_security/visualizations/covid_cases/output/covid_plot.pdf"))


Sys.setlocale("LC_TIME", "es_ES") # Changing language to adapt labels in spanish 


# Import -------------------------------------------------------------------

# Covid cases from google drive dataset
df_orig <- read_sheet(files$input,
                      col_types = "Dn") #Col types format for googlesheets 


# Importing Axia font for plots

fonts <- font_files() %>% tibble
axia_fonts <- fonts %>% filter(str_detect(family, "Axia"))
font_add(family = "Axia", regular = "Axia-Regular.otf")


# Creating copy of df 
df <- df_orig %>% arrange(ymd(df_orig$date_onset)) # Sorting to earliest to oldest



# Wrangling  -------------------------------------------------------------


df <- df %>% 
  # Moving average in a window of 7 days
  mutate(mov_aver = rollmean(cases, k = 7, fill = NA))# %>% 
  
  # Filtering new cases that ocurred 15 march 2020 to 18 september 2020)
  #filter(date_onset >= "2020-03-15" & date_onset <= "2020-09-20") 

# Rounding moving average
df$mov_aver <- round(df$mov_aver,0)
  

# How to deal with dates that dont work 

# Visualization ----------------------------------------------------------------

# Base plot 
plot <- df %>% ggplot() +
  geom_line(aes(
    x = date_onset, 
    y = mov_aver), size = .25, color = "#FFAF3A") + 
  
  # Changing y scale  
  scale_y_continuous(   #Look effect of this 
    limits = c(0, 450),
    breaks = c(seq(from = 0, to = 450, by = 50))) +
  
  # Changing x scale
  # scale https://r-graph-gallery.com/279-plotting-time-series-with-ggplot2.html
  
  scale_x_date(date_labels = "%b", # %d day, %b month, %Y year
               date_breaks = "1 month",
               limit = c(as.Date("2020-03-15"), as.Date("2020-09-20"))) #limiting axis to dates of interest 
  


# Data labels 
data_lab <- as.Date(c("2020-09-13","2020-07-13","2020-08-9")) # Dates that will be labeled

plot <- plot + geom_text(data = df %>% filter(date_onset %in% data_lab), aes(
  x = date_onset,
  y = mov_aver + 10,
  label = mov_aver, 
  vjust = 1))


plot



# Annotations
plot <- plot + annotate(
  x = as.Date("2020-03-15"), 
  y = 20, 
  label = paste("Inicio del confinamiento",
                "15 de marzo",
                "(promedio de casos diarios = 5)",sep = "\n"),
  hjust = 0,
  geom = "text", 
  color = "#444444",
  size = 3) +
  
  annotate(
    x = as.Date("2020-06-16"), 
    y = 150, 
    label = paste("Final del confinamiento de 94 dias",
                  "16 de junio 2020",
                "(promedio diario = 13)", sep = "\n"), 
    hjust = 0,
    geom = "text", 
    color = "#444444",
    size = 3) +
  
  annotate(
    x = as.Date("2020-08-01"), 
    y = 150, 
    label = paste("En este periodo los",
                "casos diarios promedio",
                "no bajaron de 181",sep = "\n"), 
    hjust = 0,
    geom = "text", 
    color = "#444444",
    size = 3) +
  
  annotate(
    x = as.Date("2020-09-18"), 
    y = 200, 
    label = paste("94 dias luego del fin del confinamiento",
                  "18 de septiembre", sep = "\n"), 
    hjust = 0,
    geom = "text", 
    color = "#444444",
    size = 3) + 
  
  # Adding arrows 
  geom_segment(
    x = as.Date("2020-03-15"),
    y = 60,
    xend = as.Date("2020-03-15"),
    yend = 150,
    lineend = "round", # See available arrow types in example above
    linejoin = "round",
    size = .8, 
    arrow = arrow(length = unit(0.2, "cm")),
    colour = "black") +
  
  geom_segment(
    x = as.Date("2020-03-15"),
    y = 60,
    xend = as.Date("2020-03-15"),
    yend = 150,
    lineend = "round", # See available arrow types in example above
    linejoin = "round",
    size = .8, 
    arrow = arrow(length = unit(0.2, "cm")),
    colour = "black") +
  
  geom_segment(
    x = as.Date("2020-03-15"),
    y = 60,
    xend = as.Date("2020-03-15"),
    yend = 150,
    lineend = "round", # See available arrow types in example above
    linejoin = "round",
    size = .8, 
    arrow = arrow(length = unit(0.2, "cm")),
    colour = "black") +
  
  geom_segment(
    x = as.Date("2020-03-15"),
    y = 60,
    xend = as.Date("2020-03-15"),
    yend = 150,
    lineend = "round", # See available arrow types in example above
    linejoin = "round",
    size = .8, 
    arrow = arrow(length = unit(0.2, "cm")),
    colour = "black")
  

# Formating 

showtext_auto()

plot <- plot +
  labs(
    title = paste("\nGráfica 1:",
                    "El confinamiento para prevenir el covid19 fue inefectivo",
                    "Promedio diario de nuevos casos de covid19",
                    "15 de marzo – 18 de septiembre de 2020",
                    "(promedio móvil de 7 dias)\n", sep = "\n"),
    y = "Promedio diario nuevos casos Covid19",
    x = " ",
    caption = "\nFuente: Departamento de Salud de Puerto (2022, septiembre 24). 
                      COVID-19 EN CIFRAS EN PUERTO RICO.\n", sep = "\n")

plot <- plot + theme(
  panel.background = element_rect(fill = "white", color = "white"),
  panel.grid = element_blank(),
  text = element_text(color = "black", family = "Axia", size = 10),
  plot.title = element_text(hjust = 0.5, size = 13),
  axis.title.y = element_text(margin = margin(r =10)),
  axis.text = element_text(family = ""))


plot
ggplotly(plot)

