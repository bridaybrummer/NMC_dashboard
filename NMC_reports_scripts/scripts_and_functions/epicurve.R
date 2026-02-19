# make a simple epicurve for a condition 


as_epicurve_excel <- function(.) {

  . +
    theme_minimal() +
    scale_color_viridis_d(option = "A") +
    theme(
      text = element_text(family = "Century Gothic", 
                          size = 10, color = "#333333"),
      panel.background = element_rect(fill = "#F8F8F8"),
      plot.background = element_rect(fill = "#F8F8F8"),
      panel.grid.major = element_line(color = NA),
      panel.grid.minor = element_line(colour = NA),
      legend.background = element_rect(fill = "#F8F8F8"),
      legend.text = element_text(color = "#333333"),
      legend.title = element_text(color = "#333333", size = 10),
      plot.title = element_text(size = 16, color = "#666666"),
      plot.subtitle = element_text(size = 12, color = "#666666"),
      plot.caption = element_text(size = 10, color = "#666666"),
      axis.ticks.x = element_line(color = "#333333"),
      axis.ticks.y = element_line(color = "#333333"),
      #legend.position = "right",
      #legend.direction = "vertical",
      #legend.key = element_blank(),
      axis.text.x = element_text(angle = 0, size = 8)
    ) #+
  #guides(color = guide_legend(nrow = 7, size = 0.2))
}


tabyl(dat = df%>%filter(condition =="Measles"), case_definition)



#df$notification_date%>%summary

#install.packages("ggforce")
library(ggforce)
library(RColorBrewer)

epicurve_df<- df %>%filter(
  condition %in% "Measles"
)%>%mutate(
       date = as_date(notification_date), 
       year = as_year(notification_date), 
       month = as_date(as_yearmonth(as_date(notification_date))),
       epiweek =as_date( as_epiweek(notification_date))
       )%>%
  group_by(epiweek, year, date, month, case_definition)%>%
  summarise(n = n())%>%
  ggplot(.,)+
  geom_bar(  aes(x =epiweek, y = n , fill = case_definition), stat = "identity",#c("darkolivegreen")
             )+
  #geom_line( aes(x =epiweek, y = n  ))
  scale_x_date(date_labels = "%W %y", 
               date_breaks = "1 weeks", 
               limits = c(min(df$notification_date), max(df$notification_date)))+
  labs( title = "Epicurve of Measles notifications", 
        fill = "Case Definition",
        x = "Epiweek Date of Notification",
        y = "Notifications (n)"
  )

#epicurve_df
#epicurve_df%>%as_epicurve_excel

my_colors <- c("#78c679", "#f46d43")  


#epicurve_df<-epicurve_df+scale_fill_manual(values = my_colors)


epicurve_df%>%as_epicurve_excel()


df$notification_date%>%summary()

my_colors1 <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f")  

my_palette <- c("#78c679", "#c2e699", "#fee08b", "#fdae61", "#f46d43")  


year_lab <- data.frame(
  from  = c(0,  11.5),
  to    = c(11.5, 23.5),
  lab   = 2022:2023,
  y_top = 2E5,  
  y_bot = 0E5
)


year_plot <- ggplot(year_lab) +
  geom_rect(fill = "#F8F8F8", color = "white",
            aes(xmin = from, xmax = to, ymin = y_bot, ymax = y_top)) +
  geom_text(aes((from+to)/2, y = (y_bot+y_top)/2, label = lab),
            vjust = 0.5, hjust = 0.5) +
  theme_void() +
  coord_cartesian(clip = "off", xlim = c(0,27), expand = FALSE) +
  labs(x = "Statistics on queries") +
  theme(plot.margin = unit(c(0,0,0,0), "lines"))


epicurve_df

epicurve_df %>%as_epicurve_excel



### Ma
