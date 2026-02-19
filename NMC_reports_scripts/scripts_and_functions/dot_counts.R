#dot counts
conflicted::conflicts_prefer(dplyr::filter)


df1$condition <- factor(df1$condition, levels = rev(unique(df1$condition)))

condition_levels_dot<- tabyl(dat = df1%>%filter(nmccategories == 1) , condition ) %>%arrange(n)%>%select(condition) %>%pull

prov_levels_dot<- tabyl(dat = df1%>%filter(nmccategories == 1) , prov_) %>%arrange(-n)%>%select(prov_) %>%pull

dot_nmc1<-df1%>%filter(nmccategories ==1, !is.na(prov_)) %>%
  mutate(prov_ = factor(prov_ , 
                        levels = prov_levels_dot[1:9]),
         condition = factor(condition , 
                            levels = condition_levels_dot))%>%
  ggplot(., aes(x = prov_, y = condition, color = condition)) +
  geom_count(show.legend = TRUE) +
  scale_size_area(max_size = 15)+
  labs(#title = paste0("Notifications of category ", df1 %>%select(nmccategories)%>%unique() %>%pull(),   " by Province"),
    x = "Province", y = "Condition") +
  theme_minimal() +
  #  scale_color_brewer(palette = "Set1")+
  theme(text = element_text(family = "Century Gothic", size = 12, color = "#333333"),
        panel.background = element_rect(fill = "#F8F8F8"),
        plot.background = element_rect(fill = "#F8F8F8"),
        panel.grid = element_line(color = "#DDDDDD"),
        legend.background = element_rect(fill = "#F8F8F8"),
        legend.text = element_text(color = "#333333"),
        legend.title = element_text(color = "#333333", size = 10),
        plot.title = element_text(size = 16, #face = "bold",
                                  color = "#666666"),
        plot.subtitle = element_text(size = 12, color = "#666666"),
        plot.caption = element_text(size = 10, color = "#666666"),
        legend.position = "right",
        legend.direction = "vertical",
        #legend.key = 
        #legend.title = element_blank()
  ) +
  guides(color = "none",
         size = "legend")+
  scale_color_viridis_d( option = "C") 

dot_nmc1

save(dot_nmc1, file = "plots/dot_nmc1.rda" )

new_levels<- df1%>%filter(nmccategories == 2) %>%select(condition ) %>%unique%>%pull%>%as.character%>%sort(decreasing   = TRUE)

condition_levels_dot<- tabyl(dat = df1%>%filter(nmccategories == 2) ,
                             condition ) %>%arrange(n)%>%select(condition) %>%pull

prov_levels_dot<- tabyl(dat = df1%>%filter(nmccategories == 2) , 
                        prov_) %>%arrange(-n)%>%select(prov_) %>%pull



dot_nmc2<-df1%>%dplyr::filter(nmccategories ==2, !is.na(prov_)) %>%
  mutate( condition = factor(condition, levels = new_levels))%>%
  mutate(prov_ = factor(prov_ , 
                        levels = prov_levels_dot[1:9]),
         condition = factor(condition , 
                            levels = condition_levels_dot))%>%
  ggplot(., aes(x = prov_, y = condition, color = condition)) +
  geom_count(show.legend = TRUE) +
  scale_size_area(max_size = 15)+
  labs(#title = paste0("Notifications of category 2 within Each Province"),
    x = "Province", y = "Condition") +
  theme_minimal() +
  #  scale_color_brewer(palette = "Set1")+
  scale_color_viridis_d( option = "C") +
  
  # i want to try different colors scales and palletes
  # scale_color_viridis_c(option = "C") +
  # scale_color_viridis_c(option = "D") +
  # scale_color_viridis_c(option = "E") +
  #scale_color_viridis_c(option = "F") +
  
  theme(text = element_text(family = "Century Gothic", size = 12, color = "#333333"),
        panel.background = element_rect(fill = "#F8F8F8"),
        plot.background = element_rect(fill = "#F8F8F8"),
        panel.grid = element_line(color = "#DDDDDD"),
        legend.background = element_rect(fill = "#F8F8F8"),
        legend.text = element_text(color = "#333333"),
        legend.title = element_text(color = "#333333", size = 10),
        plot.title = element_text(size = 16, #face = "bold",
                                  color = "#666666"),
        plot.subtitle = element_text(size = 12, color = "#666666"),
        plot.caption = element_text(size = 10, color = "#666666"),
        legend.position = "right",
        legend.direction = "vertical",
        legend.key = element_blank()
        #legend.title = element_blank()
  ) +
  guides(color = "none")

dot_nmc2
save(dot_nmc2, file = "plots/dot_nmc2.rda" )
#dot_nmc2