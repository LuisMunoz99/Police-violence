# Este es un invento


#Load packages
if(!require(pacman))install.packages("pacman")
p_load(dplyr, here, readr, lubridate, ggplot2, googlesheets4)


files <- list(input = ("1jtAk5Fdm7GLDtcmSiFseDKPgfazQAqAd0EVQ9dWjSYQ"), #Tengo que cambiarlo 
              output = here::here("public_security/visualizations/rose_diagram/output/rosediag_tq.pdf"))

# Modificar a que no sea por google docs eso no brega

# Import data 
df_orig <- read_sheet(files$input,
                      col_types = "dcDcttnn",
                      sheet = 2) # Col types format for googlesheets 


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
    color = "#444444",
    alpha = .2) + 

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
    limits = c(0, 20),
    breaks = c(7:12)) +
  

  # Polar coordinates (circular plot)
  coord_polar() +
  
  # Labels
  geom_text(data = label_df, aes(
    x = executive_order,
    y = scale + .5, 
    label = date_start_label,
    hjust = hjust), 
    color ="#444444", 
    size = 4, 
    angle = label_df$angle,
    inherit.aes = FALSE) 

  
   #reference scale labels
plt <- plt + annotate(
    x = c(27,9,19),
    y = 8, 
    label = "8 pm", 
    geom = "text", 
    color = "#444444",
  size = 4,
  fontface = "bold") +
  
    annotate(
      x = c(27,9,19),
      y = 10, 
      label = "10 pm", 
      geom = "text", 
      color = "#444444",
    size = 4,
    fontface = "bold") +
  
  annotate(
    x = c(27,9,19),
    y = 12, 
    label = "12 pm", 
    geom = "text", 
    color = "#444444",
  size = 4,
  fontface = "bold") 


# Formating 
plt <- plt + theme(
  panel.background = element_rect(fill = "white", color = "white"),
  panel.grid = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank(),
  axis.ticks = element_blank(),
  text = element_text(color = "#444444"))

plt

ggsave(files$output, plt, width=9, height=12.6)
