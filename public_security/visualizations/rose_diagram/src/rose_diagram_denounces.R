# Introduction ------------------------------------------------------------------------------
# 
# Cleaning petal diagram to present amount of police arrest and 
# denounces across executive order.
# since the start of covid19 pandemic.
#
#
# Author:  LMN 
# Date:  15-09-2022
# Organization: Kilometro Cero 



#Load packages
if(!require(pacman))install.packages("pacman")
p_load(dplyr, here, readr, lubridate, ggplot2, googlesheets4)


files <- list(input = ("1jtAk5Fdm7GLDtcmSiFseDKPgfazQAqAd0EVQ9dWjSYQ"), #Tengo que cambiarlo 
              output = here::here("public_security/visualizations/rose_diagram/output/rose_diag_ad.pdf"))

# Modificar a que no sea por google docs eso no brega

# Import data 
df_orig <- read_sheet(files$input,
                      col_types = "dcDcDnttnnnnc",
                      sheet = 3) #Col types format for googlesheets 



# Cleaning
df <- df_orig


# Labels for plot

label_df <- df

# calculate the ANGLE of the labels
label_df$angle <- 90 - 360 * (label_df$`#`- 0.5) /nrow(label_df) # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)



label_df <- label_df %>% mutate(hjust = ifelse( label_df$angle < -90, .3, 0), # calculate the alignment of labels: right or left
                                # Puedo arreglar esto con un case_when en donde los valores menores a tal y con un grado -90 esten en cierto lado poner la justificacion como me de la gana
                                angle = ifelse(label_df$angle < -90, label_df$angle+180, label_df$angle))

#label_df <- label_df %>% mutate(ifelse(mutate <= 5))
# options(repr.plot.width = 2, repr.plot.height =3)

# Base plot

plt <- ggplot(df) +
  
  # Make reference circles
  geom_hline(aes(
    yintercept = y), 
    data.frame(y = c(5,10,20,30,40,50)), # Add hline within spaces 7-12 
    color = "black",
    alpha = .3) + 
  
  # Creating columns 
  geom_col(aes(
    x = executive_order,
    y = change_denounce),
    position = "dodge2",
    show.legend = TRUE,
    width = .8,
    fill = "#FFAF3A",
    color = "black") +
  
  
  # Creating space in middle by expanding Y axis
  # scale_y_continuous(
  #limits = c(0, 35),
  #expand = c(0, 0),
  #breaks = 3) +
  
  scale_y_log10() + 
  
  
  # Polar coordinates (circular plot)
  coord_polar() +
  
  # Labels of executive orders
  geom_text(data = label_df, aes(
    x = executive_order,
    y = change_denounce + 4, 
    label = date_start_label,
    hjust = hjust), 
    color ="#444444", 
    size = 2.5, 
    angle = label_df$angle,
    inherit.aes = FALSE) +
  
  # Labels of values
  geom_text(data = label_df, aes(
    x = executive_order,
    y = change_denounce, 
    label = label_change_denounce,
    hjust = 2), # Esto me deja poner el label dentro de la grafica
    color = "#444444", 
    size = 4.5, 
    angle = label_df$angle,
    inherit.aes = FALSE,
    ) +
  
  
  # reference scale labels
  annotate(
    x = 1, 
    y = 10, 
    label = "10", 
    geom = "text", 
    color = "#444444",
    fontface = "bold"
  ) +
  annotate(
    x = 1, 
    y = 20, 
    label = "20", 
    geom = "text", 
    color = "#444444",
    fontface = "bold"
  ) + 
  annotate(
    x = 1, 
    y = 30, 
    label = "30", 
    geom = "text", 
    fontface = "bold",
  ) +
  annotate(
    x = 1, 
    y = 6, 
    label = paste( "menos ","de 5",sep = "\n"), 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 3
  ) + 
  annotate(
    x = 1,
    y = 40, 
    label = "40", 
    geom = "text", 
    color = "#444444",
    fontface = "bold",
  ) +
  annotate(
    x = 1,
    y = 50, 
    label = "50", 
    geom = "text", 
    color = "#444444",
    fontface = "bold"
  )




# Add titles 
#plt <- plt + labs(
 # title = "\nToque de queda",
 # subtitle = paste(
  #  "\nEste diagrama de rosa se presenta la hora de cierre que,",
   # "establecio cada orden ejecutiva\n",
   # sep = "\n"),
  #x = "",
  #y = "",
  #caption = "\nFuente: Ordenes ejecutivas") +
  
  
  # Customize general theme
  # Formating
  
  # Make the background white and remove extra grid lines
plt <- plt + theme(
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    text = element_text(color = "black"))

  # Set default color

# Customize the text in the title, subtitle, and caption
#plot.title = element_text(size = 13, hjust = 0.05),
#plot.subtitle = element_text(size = 11, hjust = .5),
#plot.caption = element_text(size = 7, hjust = .5))

plt
#ggsave("plot.png", plt,width=9, height=12.6)
