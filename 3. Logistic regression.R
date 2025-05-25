colnames(df)
unique(df$Occupation)
df$Turnover <- ifelse(df$Employment_Years < 8 , 1 , 0)

#Initial model and VIF test (Appendix 5 and 6)
m1 <- glm(Turnover ~ 
            Work_Hours + Sex + Age + 
            Number_Promotion + Union_Member + Aware+
            Education_Years + Working_Years +
            Sex_Promotion + Future_Promotion 
          , df, family = binomial)
summary(m1)
vif(m1)

#Model with `Union_Member` variable (Appendix 7)
m2 <- glm(Turnover ~ Sex + 
            Number_Promotion + Union_Member+
            Education_Years + Working_Years +
            Sex_Promotion + Future_Promotion 
          , df, family = binomial)
summary(m2)
vif(m2)

#Final model (Appendix 8)
model <- glm(Turnover ~ Sex + 
                         Number_Promotion +
                         Education_Years + Working_Years +
                         Sex_Promotion + Future_Promotion 
                         , df, family = binomial)
summary(model)
vif(model)
df$probability <- predict(model, df, type = "response")
df <- df[ order(df$probability, decreasing = TRUE) , ] 
df <- df %>% 
  mutate(across(probability,~round(.x,3))) 



#Select Professional employees to retain       
df %>% 
  select(ID,Occupation,Age,Sex,Employment_Years,,Education_Years,probability) %>% 
  filter(Occupation  == "Professional"& Education_Years >= 14) %>% 
  head(3)

#Select Tech/Sales employees to retain    
df %>% 
  select(ID,Occupation,Age,Sex,Employment_Years,Education_Years,probability) %>% 
  filter(Occupation  == "Tech/Sales"& Education_Years >= 14) %>% 
  head(2) 

#Select Managerial employees to retain    
df %>% 
  select(ID,Occupation,Age,Sex,Employment_Years,Education_Years,probability) %>% 
  filter(Occupation  == "Managerial" & Education_Years >= 14) %>% 
  head(2) 

#Select Production employees to retain    
df %>% 
  select(ID,Occupation,Age,Sex,Employment_Years,Education_Years,probability) %>% 
  filter(Occupation  == "Production" & Education_Years >= 14) %>% 
  head(2) 

#Select Production employees to retain    
df %>% 
  select(ID,Occupation,Age,Sex,Employment_Years,Education_Years,probability) %>% 
  filter(Occupation  == "Service"& Education_Years >= 14) %>% 
  head(1) 
  


