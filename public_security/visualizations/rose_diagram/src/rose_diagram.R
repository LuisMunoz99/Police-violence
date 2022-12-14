# Original script


#Load packages
if(!require(pacman))install.packages("pacman")
p_load(dplyr, here, readr, lubridate, ggplot2, googlesheets4)


files <- list(input = ("1jtAk5Fdm7GLDtcmSiFseDKPgfazQAqAd0EVQ9dWjSYQ"), #Tengo que cambiarlo 
              output = here::here("public_security/visualizations/rose_diagram/output/rose_diag_tq.pdf"))

# Modificar a que no sea por google docs eso no brega

# Import data 
df_orig <- read_sheet(files$input,
                      col_types = "dcDcttnn",
                      sheet = 1) #Col types format for googlesheets 


# Cleaning
df <- df_orig

df <- df %>% mutate(hour_start = # Changing 24hrs format to 12hrs
                      format(strptime(hour_start, "%Y-%m-%d %H:%M:%S"), "%I:%M %p"), 
                    executive_order = as.factor(executive_order)) 



# Labels for plot

label_df <- df

# calculate the ANGLE of the labels
label_df$angle <- 90 - 360 * (label_df$`#`- 0.5) /nrow(label_df) # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)



label_df <- label_df %>% mutate(hjust = ifelse( label_df$angle < -90, 1, 0), # calculate the alignment of labels: right or left
                                angle = ifelse(label_df$angle < -90, label_df$angle+180, label_df$angle))


# Base plot

plt <- ggplot(df) +
  
  # Make reference circles
  geom_hline(aes(
    yintercept = y), 
    data.frame(y = c(7:12)), # Add hline within spaces 7-12 
    color = "black",
    alpha = .3) + 
  
  # Creating columns 
  geom_col(aes(
    x = executive_order,
    y = scale),
    position = "dodge2",
    show.legend = TRUE,
    width = .8,
    fill = "#FFAF3A",
    color = "black") +
  
  
  # Creating space in middle by expanding Y axis
  scale_y_continuous(
    limits = c(-1, 12),
    expand = c(0, 0),
    breaks = c(7:12)) +
  
  
  # Polar coordinates (circular plot)
  coord_polar() +
  
  # Labels
  geom_text(data = label_df, aes(
    x = executive_order,
    y = arrest_day + .5, 
    label = start,
    hjust = hjust), 
    color ="black", 
    size = 4, 
    angle = label_df$angle,
    inherit.aes = FALSE) +
  
  
  # reference scale labels
  annotate(
    x = 9,
    y = 7, 
    label = "7 pm", 
    geom = "text", 
    color = "gray12",
    angle = 45
  ) +
  annotate(
    x = 9, 
    y = 8, 
    label = "8 pm", 
    geom = "text", 
    color = "gray12",
    angle = 45
  ) +
  annotate(
    x = 9, 
    y = 9, 
    label = "9 pm", 
    geom = "text", 
    color = "gray12",
    angle = 45
  ) +
  annotate(
    x = 9, 
    y = 10, 
    label = "10 pm", 
    geom = "text", 
    color = "gray12",
    angle = 45
  ) +
  annotate(
    x = 9, 
    y = 11, 
    label = "11 pm", 
    geom = "text", 
    color = "gray12",
    angle = 45
  ) +
  annotate(
    x = 9, 
    y = 12, 
    label = "12 pm", 
    geom = "text", 
    color = "gray12",
    angle = 45
  ) 


plt




# Add titles 
plt <- plt + labs(
  title = "\nToque de queda",
  subtitle = paste(
    "\nEste diagrama de rosa se presenta la hora de cierre que,",
    "establecio cada orden ejecutiva\n",
    sep = "\n"),
  x = "",
  y = "",
  caption = "\nFuente: Ordenes ejecutivas") +
  
  
  # Customize general theme
  # Formating
  
  # Make the background white and remove extra grid lines
  theme(
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    text = element_text(color = "black"),  # Set default color
    
    # Customize the text in the title, subtitle, and caption
    plot.title = element_text(size = 13, hjust = 0.05),
    plot.subtitle = element_text(size = 11, hjust = .5),
    plot.caption = element_text(size = 7, hjust = .5))

plt
ggsave("plot.png", plt,width=9, height=12.6)
