

#Scatter Plot of All Numeric Variables with Employment Years(Appendix 2)
ggplotly(df %>% 
  pivot_longer(cols =c(Work_Hours,Age,Education_Years,Working_Years,Number_Promotion),
               names_to = "num_var",
               values_to = "value") %>% 
  ggplot(aes(x = value, y = Employment_Years))+
  geom_point(size = 1, alpha = 0.5, color = major_color)+
  geom_smooth(method = "lm", se = FALSE)+
  facet_wrap(~num_var, scales = "free",
             labeller = labeller(num_var = function(x) gsub("_"," ",x)))+
  labs(x = NULL, y = NULL, title = "Scatter Plot of All Numeric Variables with Employment Years")+
  theme_classic()+
  theme(
    plot.title = element_text(size = 15, color = major_color, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 11, color = major_color),
    axis.text = element_text(size = 9, color = major_color),
    strip.text = element_text(size = 11, color = major_color, face = "bold"), 
    strip.background = element_rect(color = major_color)))
  
#Perform winzorization for erroneous values (Appendix 3)
df <- df %>% 
mutate(Employment_Years = case_when(Employment_Years == 52.25 ~ 45,
                                    TRUE ~ Employment_Years)) 

#Perspective of Each Gender about Gender Bias in Promotions (Appendix 4)
  df %>% 
    group_by(Sex_Promotion,Sex) %>% 
    summarise(Count = n(),
              .groups = "drop") %>% 
    ggplot(aes(x = Sex_Promotion, y = Count, fill = Sex))+
    geom_bar(stat = "identity")+
    scale_fill_manual(values = c("Male" = major_color, "Female" = minor_color))+
    labs(x = NULL, y = NULL, title = "Perspective of Each Gender about Gender Bias in Promotions")+
    theme_minimal()+
    theme(
      plot.title = element_text(size = 15, color = major_color, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 11, color = major_color),
      axis.text = element_text(size = 9, color = major_color),
      strip.text = element_text(size = 11, color = major_color, face = "bold"), 
      strip.background = element_rect(color = major_color),
      legend.position = "bottom",
      legend.title = element_text(size = 11, color = major_color),
      legend.text = element_text(size = 9, color = major_color))

  







