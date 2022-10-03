# Este es un invento


#Load packages
if(!require(pacman))install.packages("pacman")
p_load(dplyr, here, readr, lubridate, ggplot2, googlesheets4)


files <- list(input = ("1jtAk5Fdm7GLDtcmSiFseDKPgfazQAqAd0EVQ9dWjSYQ"), #Tengo que cambiarlo 
              output = here::here("public_security/visualizations/rose_diagram/output/rose_diag.pdf"))

# Modificar a que no sea por google docs eso no brega

# Import data 
df_orig <- read_sheet(files$input,
                      col_types = "dcDcttnn") #Col types format for googlesheets 


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
    data.frame(y = c(7:12)), # Add hline within spaces 7-12 
    color = "#555555") + 

  # Creating columns 
  geom_col(aes(
      x = executive_order,
      y = scale),
    position = "dodge2",
    show.legend = TRUE,
    width = .8,
    fill = "#FFAF3A") +

  
  # Creating space in middle by expanding Y axis
  scale_y_continuous(
    limits = c(-1, 12),
    expand = c(0, 0),
    breaks = c(7:12)) +
  
  # Formating
  theme(
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    axis.text.x=element_blank()) +

  # Polar coordinates (circular plot)
  coord_polar() +
  
  # Labels 
  annotate(
    x = df$executive_order, 
    y = df$scale - 2,
    label = df$hour_start_label,
    geom = "text",
    angle = 360, # Puedo crear una variable de angle y aplicarla aqui 
    color = "black",
    size = 3.5)
  

# ------------------------------------------------------------------------------

plt <- plt +
  # Esto lo puedo hacer para añadir labels manuales y con angulos 
  # Annotate the bars and the lollipops so the reader understands the scaling
  annotate(
    x = 2, 
    y = 10,
    label = "Mean Elevation Gain\n[FASL]",
    geom = "text",
    angle = -67.5,
    color = "gray12",
    size = 2.5,
    family = "Bell MT"
  ) +
  annotate(
    x = 11, 
    y = 12,
    label = "Cummulative Length [FT]",
    geom = "text",
    angle = 23,
    color = "gray12",
    size = 2.5#,
    #family = "Bell MT"
  ) +
  # Annotate custom scale inside plot
  annotate(
    x = 11.7, 
    y = 10, 
    label = "1000", 
    geom = "text", 
    color = "gray12", 
    family = "Bell MT"
  ) +
  annotate(
    x = 11.7, 
    y = 8, 
    label = "2000", 
    geom = "text", 
    color = "gray12", 
    family = "Bell MT"
  ) +
  annotate(
    x = 11.7, 
    y = 5, 
    label = "3000", 
    geom = "text", 
    color = "gray12", 
    family = "Bell MT"
  ) +
  # Scale y axis so bars don't start in the center
  scale_y_continuous(
    limits = c(-1500, 3500),
    expand = c(0, 0),
    breaks = c(0, 1000, 2000, 3000)
  ) + 
  # New fill and legend title for number of tracks per region
  scale_fill_gradientn(
    "Amount of Tracks",
    colours = c( "#6C5B7B","#C06C84","#F67280","#F8B195")
  ) +
  # Make the guide for the fill discrete
  guides(
    fill = guide_colorsteps(
      barwidth = 15, barheight = .5, title.position = "top", title.hjust = .5
    )
  ) +
  theme(
    # Remove axis ticks and text
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    # Use gray text for the region names
    axis.text.x = element_text(color = "gray12", size = 12),
    # Move the legend to the bottom
    legend.position = "bottom",
  )


plt



# Add labels
plt + labs(
  title = "\nHiking Locations in Washington",
  subtitle = paste(
    "\nThis Visualisation shows the cummulative length of tracks,",
    "the amount of tracks and the mean gain in elevation per location.\n",
    "If you are an experienced hiker, you might want to go",
    "to the North Cascades since there are a lot of tracks,",
    "higher elevations and total length to overcome.",
    sep = "\n"
  ),
  caption = "\n\nData Visualisation by Tobias Stalder\ntobias-stalder.netlify.app\nSource: TidyX Crew (Ellis Hughes, Patrick Ward)\nLink to Data: 
  github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-11-24/readme.md") +
  # Customize general theme
  theme(
    
    # Set default color and font family for the text
    text = element_text(color = "gray12", family = "Bell MT"),
    
    # Customize the text in the title, subtitle, and caption
    plot.title = element_text(face = "bold", size = 25, hjust = 0.05),
    plot.subtitle = element_text(size = 14, hjust = 0.05),
    plot.caption = element_text(size = 10, hjust = .5),
    
    # Make the background white and remove extra grid lines
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank()
  )
# Use `ggsave("plot.png", plt,width=9, height=12.6)` to save it as in the output
plt
