# Introduction ------------------------------------------------------------------------------
# 
# Cleaning petal diagram to present the duration of curfuews across time
# since the start of covid19 pandemic.
#
#
# Author:  LMN 
# Date:  15-09-2022
# Organization: Kilometro Cero 

# Set up ----------------------------------------------------------------------



#Load packages
if(!require(pacman))install.packages("pacman")
p_load(dplyr, here, readr, lubridate, ggplot2, googlesheets4)


files <- list(input = ("1jtAk5Fdm7GLDtcmSiFseDKPgfazQAqAd0EVQ9dWjSYQ"), #Tengo que cambiarlo 
              output = here::here("public_security/visualizations/rose_diagram/output/rose_diag.pdf"))
            
#Modificar a que no sea por google docs eso no brega

# Import data -------------------------------------------------------------------
df_orig <- read_sheet(files$input,
                      col_types = "dcDcttnn") #Col types format for googlesheets 


# Cleaning
df <- df_orig

df <- df %>% mutate(hour_start = format(strptime(hour_start, "%Y-%m-%d %H:%M:%S"), "%I:%M %p")) # Changing 24hrs format to 12hrs

                    


# Visualization ----------------------------------------------------------------


#df_hour <- data.frame(value = 1:12,
                      #hour_start_label = c("1:00 pm","2:00 pm","3:00 pm",
                                           #"4:00 pm","5:00 pm","6:00 ",
                                           #"7:00 pm","8:00 pm","9:00 pm",
                                           #"10:00 pm","11:00 pm","12:00 am"))
# Tengo que crear un script de los inicios de este banco de datos donde cree este df_hour para asignar el label, esto no vino con el banco
# Solo tenia las ordenes ejecutivas sin el label eso lo cree. Mentira no tenia la escala 



# Labels (SEP-21-22)

df %>% ggplot(aes(executive_order, scale)) + 
  geom_col(aes(x = executive_order), width = .8, position = "identity", 
           fill = "white", color = "black", size = .25) + # Polar coordinates 
  #scale_y_sqrt() + not required i think
  theme_void() +
  geom_text(aes(label = hour_start_label), vjust = 0.8, hjust = 0.5, colour = "black") + #Hour label 
  theme(axis.text.x = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = alpha("white", 0.5)),
        plot.margin = unit(c(5, 5, 5, 5), "pt"),
        plot.title = element_text(vjust = 5)) + 
  ylim(-2,13) + # Space within circle
  #geom_segment(aes(x, y, xend = xend, yend = yend, colour = "curve")) +
  #geom_segment(aes(x = 12, y = 13, xend = 0, yend = 13, colour = "curve")) +
  # Verify grid:: to verify parameters of the curve in polar diagrams 
  coord_polar() +

            
  geom_vline(xintercept = c(0.4, 7.4, 16.6)) 


#+

  #geom_text(aes(.56, 4.1, label = "- 9")) +
  #geom_text(aes(.578, 5.1, label = "- 10")) +
  #geom_text(aes(.575, 6.1, label = "- 11")) +
  #geom_text(aes(.57, 7.1, label = "- 12"))
  #ggtitle("Curfews first 6 months (Hours labels)")

# Test
  main <- df %>% ggplot(aes(executive_order, scale)) +
    geom_segment(
      aes(x = executive_order, xend = executive_order, y = 0, yend = scale), 
      size = 1.2
    ) +
    # This rect is converted into the inner circle where we're going to place text
    # when converting the plot to circular coordinates.
    geom_rect(
      aes(xmin = 1, xmax = 501, ymin = 0, ymax = plus), 
      fill = "grey97", color = "grey97"
    ) + 
    
    # Add our custom grid lines for the radial axis.
    # These lines indicate one day, one week, one month and one year.
    geom_hline(aes(yintercept = 9, color = "grey88")) +
    geom_hline(aes(yintercept = 10, color = "grey85")) +
    geom_hline(aes(yintercept = 11, color = "grey82")) +
    geom_hline(aes(yintercept = 12, color = "grey79")) +
  
    
    # Polar axis (y-axis) is in log10 scale
    scale_y_sqrt(expand = c(0, 0)) +
    
    
    # Make it circular!
    coord_polar() 
  
  main
  
  
  
  
  
  
# Second semester
months_12 %>%
  left_join(df_hour, by = "hour_start_label") %>% 
  ggplot(aes(exxecutive_order, value)) + 
  geom_col(aes(fill = exxecutive_order), width = 1, position = "identity", 
           fill = "white", color = "black", size = .25) + 
  coord_polar() +
  geom_text(aes(label = hour_start_label), vjust = 0, hjust = 0, colour = "black") +
  scale_y_sqrt() +
  theme_void() +
  theme(axis.text.x = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = alpha("#EDDCD2", 0.5)),
        plot.margin = unit(c(15, 15, 15, 15), "pt"),
        plot.title = element_text(vjust = 5)) +
  geom_vline(xintercept = 4.5) +
  geom_vline(xintercept = 7.5) +
  geom_vline(xintercept = .5) +
  geom_text(aes(.56, 4, label = "- 9")) +
  geom_text(aes(.578, 5, label = "- 10")) +
  geom_text(aes(.575, 6, label = "- 11")) +
  geom_text(aes(.57, 7, label = "- 12"))
  ggtitle("Curfews last 6 months (Hours labels)") 


# 2 = 7:00 pm
# 3 = 8:00 pm
# 4 = 9:00 pm
# 5 = 10:00 pm
# 6 = 11:00 pm
# 7 = 12:00 pm





dev.off() 



write.xlsx(x,"/Users/luismunoz/Documents/Documents MacBook Air/git/Police-Violence/toques_queda.xl")
