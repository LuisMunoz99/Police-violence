#Load packages
if(!require(pacman))install.packages("pacman")
p_load(dplyr, here, readr, lubridate, ggplot2, googlesheets4)


files <- list(input = ("1jtAk5Fdm7GLDtcmSiFseDKPgfazQAqAd0EVQ9dWjSYQ"), #Tengo que cambiarlo 
              output = here::here("public_security/visualizations/rose_diagram/output/rose_diag_arres.pdf"))

# Modificar a que no sea por google docs eso no brega

# Import data 
df_orig <- read_sheet(files$input,
                      col_types = "dcDcttnnn") #Col types format for googlesheets 


# Cleaning
df <- df_orig

df <- df %>% mutate(hour_start = format(strptime(hour_start, "%Y-%m-%d %H:%M:%S"), "%I:%M %p"),
                    executive_order = as.factor(executive_order)) # Changing 24hrs format to 12hrs


# Aggregating datas

# Base plot
plt <- ggplot(df) +
  
  # Make custom panel grid
  geom_hline(aes(
    yintercept = y), 
    data.frame(y = c(100,200,300,400,500)), # Add hline within spaces 7-12 
    color = "#555555") + 
  
  # Creating columns 
  geom_col(aes(
    x = executive_order,
    y = arresto),
    position = "dodge2",
    show.legend = TRUE,
    width = .8,
    fill = "#FF0000") +
  
  
  # Creating space in middle by expanding Y axis
  scale_y_continuous(
    limits = c(-1, 500),
    expand = c(0, 0),
    breaks = seq(0, 500, by = 100)) +
  
  # Formating
  theme(
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    axis.text.x=element_blank()) +
  
  # Polar coordinates (circular plot)
  coord_polar() #+
  
  # Labels 
  annotate(
    x = df$executive_order, 
    y = df$arresto - 2,
    label = df$executive_order, 
    geom = "text",
    angle = 360, # Puedo crear una variable de angle y aplicarla aqui 
    color = "black",
    size = 3.5)


# label fecha abreviada (MAR-15 etc. solo 2020 y 2021)
plt
