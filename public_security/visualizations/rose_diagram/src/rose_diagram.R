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
p_load(dplyr, here, readr, lubridate, ggplot2)


files <- list(input = ("public_security/visualizations/rose_diagram/input/toques_queda.csv"), #Tengo que cambiarlo 
              output = here::here("public_security/visualizations/rose_diagram/output/rose_diag.pdf"))
            

              


# Import data -------------------------------------------------------------------
df_orig <- read_csv(files$input,
               col_types = cols(.default = "c", hour_start = "t", hour_finish = "t",
                                '#' = "f", hour_duration = "n"))

df_months <- data.frame(month_num = c("01-20", "02-20","03-20","04-20","05-20","06-20",
                                      "07-20","08-20","09-20","10-20","11-20","12-20",
                                      "01-21","02-21","03-21","04-21","05-21"),
                        month_span = c("ene-20", "feb-20","mar-20","abr-20","may-20","jun-20",
                                         "jul-20","ago-20","sep-20","oct-20","nov-20","dic-20",
                                         "ene-21","feb-21","mar-21","abr-21","may-21"))




# Cleaning
df <- df_orig[1:7] 

df <- df %>% mutate(date_start = as.Date(date_start, "%m/%d/%y"),
                    hour_start = format(strptime(hour_start, "%H:%M:%S"), "%I:%M %p"), # Changing 24hrs format to 12hrs
                    month_start = format(date_start, "%m-%y"),
                    month_abr = format(date_start, "%m-%y", label = TRUE),
                    hour_free = 24 - hour_duration) %>% 
  left_join(df_months, by = c("month_start" = "month_num"))

months_6 <- df %>% filter(date_start >= "2020-03-15" & date_start <= "2020-09-21")
months_12 <- df %>% filter(date_start > "2020-09-21")

# Visualization ----------------------------------------------------------------

# Tienen que haber unas categorias para ubicarlas en lugar (los meses)

months_abr <- df_months$month_abr






df_hour <- data.frame(value = 2:7,
                      hour_start_label = c("7:00 pm","8:00 pm","9:00 pm",
                                           "10:00 pm","11:00 pm","12:00 am"))



pdf(files$output)
# No labels no lines 

# First semester
months_6 %>%
  left_join(df_hour, by = "hour_start_label") %>% ggplot(aes(exxecutive_order, value)) + 
  geom_col(aes(fill = exxecutive_order), width = 1, position = "identity", 
           fill = "white", color = "black", size = .25) + 
  coord_polar() +
  scale_y_sqrt() +
  theme_void() +
  theme(axis.text.x = element_text(size = 9),
        strip.text = element_text(size = 11),
        legend.position = "none",
        plot.background = element_rect(fill = alpha("white", 0.5)),
        plot.margin = unit(c(15, 15, 15, 15), "pt"),
        plot.title = element_text(vjust = 5)) +
  ggtitle("Curfews first 6 months (No labels)")

# Second semester
months_12 %>%
  left_join(df_hour, by = "hour_start_label") %>% ggplot(aes(exxecutive_order, value)) + 
  geom_col(aes(fill = exxecutive_order), width = 1, position = "identity", 
           fill = "white", color = "black", size = .25) + 
  coord_polar() +
  scale_y_sqrt() +
  theme_void() +
  theme(axis.text.x = element_text(size = 9),
        strip.text = element_text(size = 11),
        legend.position = "none",
        plot.background = element_rect(fill = alpha("white", 0.5)),
        plot.margin = unit(c(15, 15, 15, 15), "pt"),
        plot.title = element_text(vjust = 5)) +
  ggtitle("Curfews last 6 months (No labels)") 

# All months 
df %>%
  left_join(df_hour, by = "hour_start_label") %>% ggplot(aes(exxecutive_order, value)) + 
  geom_col(aes(fill = exxecutive_order), width = 1, position = "identity", 
           fill = "white", color = "black", size = .25) + 
  coord_polar() +
  scale_y_sqrt() +
  theme_void() + 
  theme(axis.text.x = element_text(size = 9),
        strip.text = element_text(size = 11),
        legend.position = "none",
        plot.background = element_rect(fill = alpha("white", 0.5)),
        plot.margin = unit(c(15, 15, 15, 15), "pt"),
        plot.title = element_text(vjust = 5)) +
  ggtitle("Curfews all months (No labels)")

# Labels 
months_6 %>%
  left_join(df_hour, by = "hour_start_label") %>% ggplot(aes(exxecutive_order, value)) + 
  geom_col(aes(fill = exxecutive_order), width = 1, position = "identity", 
           fill = "white", color = "black", size = .25) + 
  coord_polar() +
  scale_y_sqrt() +
  theme_void() +
  geom_text(aes(label = hour_start_label), vjust = 0.8, hjust = 0.5, colour = "black") +
  theme(axis.text.x = element_text(size = 9),
        strip.text = element_text(size = 11),
        legend.position = "none",
        plot.background = element_rect(fill = alpha("white", 0.5)),
        plot.margin = unit(c(15, 15, 15, 15), "pt"),
        plot.title = element_text(vjust = 5)) +
  ggtitle("Curfews first 6 months (Hours labels)")

# Second semester
months_12 %>%
  left_join(df_hour, by = "hour_start_label") %>% ggplot(aes(exxecutive_order, value)) + 
  geom_col(aes(fill = exxecutive_order), width = 1, position = "identity", 
           fill = "white", color = "black", size = .25) + 
  coord_polar() +
  geom_text(aes(label = hour_start_label), vjust = 0.8, hjust = 0.5, colour = "black") +
  scale_y_sqrt() +
  theme_void() +
  theme(axis.text.x = element_text(size = 9),
        strip.text = element_text(size = 11),
        legend.position = "none",
        plot.background = element_rect(fill = alpha("white", 0.5)),
        plot.margin = unit(c(15, 15, 15, 15), "pt"),
        plot.title = element_text(vjust = 5)) +
  ggtitle("Curfews last 6 months (Hours labels)") 





df %>%
  left_join(df_hour, by = "hour_start_label") %>% ggplot(aes(exxecutive_order, value)) + 
  geom_col(aes(fill = exxecutive_order), width = 1, position = "identity", 
           fill = "white", color = "black", size = .25) + 
  coord_polar() +
  geom_text(aes(label = hour_start_label), vjust = 0.8, hjust = 0.5, colour = "black") +
  scale_y_sqrt() +
  theme_void() +
  theme(axis.text.x = element_text(size = 9),
        strip.text = element_text(size = 11),
        legend.position = "none",
        plot.background = element_rect(fill = alpha("white", 0.5)),
        plot.margin = unit(c(15, 15, 15, 15), "pt"),
        plot.title = element_text(vjust = 5)) +
  ggtitle("Curfews all months (Hours labels)")


# Reference lines
months_6 %>%
  left_join(df_hour, by = "hour_start_label") %>% ggplot(aes(exxecutive_order, value)) + 
  geom_col(aes(fill = exxecutive_order), width = 1, position = "identity", 
           fill = "white", color = "black", size = .25) + 
  coord_polar() +
  scale_y_sqrt() +
  theme_void() + 
  geom_hline(aes(yintercept = value)) + 
  theme(axis.text.x = element_text(size = 9),
        strip.text = element_text(size = 11),
        legend.position = "none",
        plot.background = element_rect(fill = alpha("white", 0.5)),
        plot.margin = unit(c(15, 15, 15, 15), "pt"),
        plot.title = element_text(vjust = 5)) +
  ggtitle("Curfews first 6 months (Reference lines)")

# Second semester
months_12 %>%
  left_join(df_hour, by = "hour_start_label") %>% ggplot(aes(exxecutive_order, value)) + 
  geom_col(aes(fill = exxecutive_order), width = 1, position = "identity", 
           fill = "white", color = "black", size = .25) + 
  coord_polar() + geom_hline(aes(yintercept = value)) + 
  scale_y_sqrt() +
  theme_void() +
  theme(axis.text.x = element_text(size = 9),
        strip.text = element_text(size = 11),
        legend.position = "none",
        plot.background = element_rect(fill = alpha("white", 0.5)),
        plot.margin = unit(c(15, 15, 15, 15), "pt"),
        plot.title = element_text(vjust = 5)) +
  ggtitle("Curfews last 6 months (Reference lines)")





df %>%
  left_join(df_hour, by = "hour_start_label") %>% ggplot(aes(exxecutive_order, value)) + 
  geom_col(aes(fill = exxecutive_order), width = 1, position = "identity", 
           fill = "white", color = "black", size = .25) + 
  coord_polar() +
  scale_y_sqrt() +
  theme_void() + geom_hline(aes(yintercept = value)) + 
  theme(axis.text.x = element_text(size = 9),
        strip.text = element_text(size = 11),
        legend.position = "none",
        plot.background = element_rect(fill = alpha("white", 0.5)),
        plot.margin = unit(c(15, 15, 15, 15), "pt"),
        plot.title = element_text(vjust = 5)) +
  ggtitle("Curfews all months (Reference lines)")


dev.off() 

