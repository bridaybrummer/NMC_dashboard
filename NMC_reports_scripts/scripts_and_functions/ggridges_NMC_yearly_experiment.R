## ggplot experiments_yearly report

install.packages("ggridges")
library(ggridges)


load("nmc_2023_clean_current.rda")

load("nmc.rda")

nmc 

nmc$notification_date %>%summary() 

as_excel <- function(.) {
  . +
    theme_minimal() +
    #scale_x_date(date_labels = "%b \n '%y", date_breaks = "6 months", limits = c(min, max)) +
    #scale_color_viridis_d(option = "B") +
    theme(
      text = element_text(family = "Century Gothic", 
                          size = 12, color = "#333333"),
      panel.background = element_rect(fill = "#F8F8F8"),
      plot.background = element_rect(fill = "#F8F8F8"),
      panel.grid = element_line(color = "#DDDDDD"),
      legend.background = element_rect(fill = "#F8F8F8"),
      legend.text = element_text(color = "#333333"),
      legend.title = element_text(color = "#333333", size = 10),
      plot.title = element_text(size = 16, color = "#666666"),
      plot.subtitle = element_text(size = 12, color = "#666666"),
      plot.caption = element_text(size = 10, color = "#666666"),
      #legend.position = "right",
      #legend.direction = "vertical",
      #legend.key = element_blank(),
      axis.text.x = element_text(angle = 30)
    ) #+
  #guides(color = guide_legend(nrow = 7, size = 0.2))
}

nmc_2023_clean_current

df1$condition%>%unique

nmc_2023_clean_current%>%filter(condition == "Cholera") %>%
  group_by(notification_date) %>%
  summarise(n= n())%>%
  group_by(notification_date) %>%
  ggplot(., aes( x= notification_date, y = n ))+
  geom_line()+
  geom_smooth()+
  stat_density(aes(y = ..count..), fill = "lightblue", alpha = 0.5) 

nmc %>%
  filter(condition == "Cholera") %>%
  group_by(notification_date) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = notification_date, y = n)) +
  geom_line(color = "blue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  geom_density(aes(y = ..count..), fill = "lightblue", alpha = 0.5) +
  labs(
    title = "Cholera Cases Over Time",
    x = "Notification Date",
    y = "Count"
  ) +
  theme_minimal() +
  scale_y_continuous(
    sec.axis = sec_axis(~. *1000, name = "Density", labels = NULL),
    name = "Count"
  )


conditions_list <- nmc$condition%>%unique

nmc %>%filter(nmccategories  ==1 )%>%
  mutate(notification_date = as_date(notification_date))%>%
  group_by(condition, notification_date) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = notification_date, y = n)) +
  geom_line() +
  geom_smooth()+
  #geom_smooth(method = "lm", color = "red")+
  facet_wrap(~condition, scales = "free_y", ncol = 1, strip.position = "left") +
  labs(title = "Notifications Over Time by Condition",
       x = "Notification Date",
       y = "") +
  theme_minimal()+
  scale_x_date(date_labels = "%b", date_breaks = "1 month", limits = c(min(nmc$notification_date), max(nmc$notification_date)) 
  ) +
  
  theme(
    text = element_text(family = "Century Gothic", size = 12, color = "#333333"),
    panel.background = element_rect(fill = "#F8F8F8"),
    plot.background = element_rect(fill = "#F8F8F8"),
    panel.grid.major = element_line(linewidth  = 0.5, colour = '#DDDDDD'),
    panel.grid.minor = element_line(colour = NA),
    #panel.grid = element_line(color = c(NA,"#DDDDDD")), # Adjust or remove this line
    legend.background = element_rect(fill = "#F8F8F8"),
    legend.text = element_text(color = "#333333"),
    legend.title = element_text(color = "#333333", size = 10),
    plot.title = element_text(size = 16, color = "#666666"),
    plot.subtitle = element_text(size = 12, color = "#666666"),
    plot.caption = element_text(size = 10, color = "#666666"),
    legend.position = "bottom",
    legend.key = element_blank()
  ) +
  #guides(color = guide_legend(nrow = 5))
  theme(
    panel.spacing.y = unit(2, "lines"),
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 0, color = 'black'), #THE LINE THAT IS EDITED
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank())



nmc
nmc$condition %>%unique%>%sort(., decreasing = T)

nmc$notification_date%>%summary()

peak_times <- nmc %>%
  filter(condition %in% enteric)%>%
  #filter(nmccategories == 1)%>%
  group_by(condition, Month_notification)%>%
  summarise(n = n())%>%
  summarize(peak_time = Month_notification[which.max(n)])

# Order the cases based on their peak times
ordered_cases <- peak_times %>%
  arrange(., desc(peak_time)) %>%
  pull(condition)
ordered_cases


ridges_plot<- nmc %>%
  filter(
    condition %in% ordered_cases
    #nmccategories == 1
  ) %>%
  mutate(condition = factor( condition, levels = c(ordered_cases)))%>%
  #mutate(condition = factor(condition, levels = c(nmc$condition %>% unique() %>% sort(., decreasing = TRUE)))) %>%

  ggplot(aes(x = notification_date, y = condition, fill = condition)) +
  
  ggridges::geom_density_ridges_gradient(show.legend = FALSE,
                                         #panel_scaling = FALSE,
                                         scale = 2) +
  #xlim( min(nmc$notification_date), max(nmc$notification_date))+
  scale_x_date(date_labels = "%b", date_breaks = "1 month", limits = c(min(nmc$notification_date), max(nmc$notification_date)) 
  )   +
  theme_minimal()+
  
  theme(
    text = element_text(#family = "Century Gothic", 
                        size = 12, color = "#333333"),
    panel.background = element_rect(fill = "#F8F8F8"),
    plot.background = element_rect(fill = "#F8F8F8"),
    panel.grid.major = element_line(linewidth  = 0.5, colour = 'grey55'),
    panel.grid.minor = element_line(colour = NA),
    #panel.grid = element_line(color = c(NA,"#DDDDDD")), # Adjust or remove this line
    legend.background = element_rect(fill = "#F8F8F8"),
    legend.text = element_text(color = "#333333"),
    legend.title = element_text(color = "#333333", size = 10),
    plot.title = element_text(size = 16, color = "#666666"),
    plot.subtitle = element_text(size = 12, color = "#666666"),
    plot.caption = element_text(size = 10, color = "#666666"),
    legend.position = "bottom",
    legend.key = element_blank()
  ) +
    #guides(color = guide_legend(nrow = 5))
    theme(
      panel.spacing.y = unit(2, "lines"),
      strip.text.y.left = element_text(angle = 0, color = 'black'), #THE LINE THAT IS EDITED
      )

ridges_plot
ridges_plot%>%as_excel(.)

# make some related conditions such as enteric or vaccine preventable. 
df$condition%>%unique

vaccine_preventable <- c("Diphtheria", "Measles", "Pertussis", "Rubella", "Meningococcal Disease",  "Congenital rubella syndrome"  )

enteric <- c("Waterborne illness outbreak - UNDEFINED", "Shiga toxin-producing Escherichia coli", "Shigellosis","Non-typhoidal Salmonellosis", "Legionellosis", "Food borne illness outbreak" ,"Cholera" )

arboviral <- c(nmc %>%filter( grepl("arbo", ignore.case =TRUE,  condition) )%>%select(condition )%>%unique %>%pull, "Malaria")
arboviral

tb <- nmc %>%filter( grepl("tuberc", ignore.case =TRUE,  condition) )%>%select(condition )%>%unique %>%pull

hepat <- nmc %>%filter( grepl("hepat", ignore.case =TRUE,  condition) )%>%select(condition )%>%unique %>%pull
hepat


# it ould be great to order the levels by which peaked earliest. 
# we would need to create a dataframe wiht the probability per month. 
# then see something....it seems quite complicated. 

peak_times


