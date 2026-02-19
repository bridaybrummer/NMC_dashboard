#ggridges function 



conditions_ridgeplot<-function(data = data, conditions_levelled = conditions_levelled, jittered_points = TRUE) {
  
  plot<- data%>%
    mutate(year = as_year(notification_date), 
           month = month(notification_date))%>%
    filter(
      condition %in% conditions_levelled
      #nmccategories == 1
    ) %>%
    mutate(condition = factor( condition, levels = c(conditions_levelled)))%>%
    #mutate(condition = factor(condition, levels = c(nmc$condition %>% unique() %>% sort(., decreasing = TRUE)))) %>%
    
    ggplot(aes(x = notification_date, y = condition, fill = condition)) +
    
    ggridges::stat_density_ridges(show.legend = FALSE,
                                           panel_scaling = TRUE,
                                           scale = 1.5, 
                                           alpha = 0.75,
                                  jittered_points = jittered_points,
                                  #position = position_points_jitter(width = 0.05, height = 0),
                                  point_shape = '|', point_size = 2, point_alpha = 0.5 ) +
    #xlim( min(nmc$notification_date), max(nmc$notification_date))+
    scale_x_date(date_labels = "%b", date_breaks = "1 month", limits = as_date(c(min(data$notification_date), max(data$notification_date))) 
    )   +
    theme_minimal()+
    
    theme(
      text = element_text(#family = "Century Gothic", 
        size = 12, color = "#333333"),
      panel.background = element_rect(fill = "#F8F8F8"),
      plot.background = element_rect(fill = "#F8F8F8"),
      panel.grid.major = element_line(linewidth  = 0.5, colour = 'grey55'),
      panel.grid.minor = element_line(colour = NA),
    ) +
    #guides(color = guide_legend(nrow = 5))
    theme(
      panel.spacing.y = unit(2, "lines"),
      strip.text.y.left = element_text(angle = 0, color = 'black') #THE LINE THAT IS EDITED
    )+
    scale_x_continuous(expand = c(0,0))
  return(plot) 
}

