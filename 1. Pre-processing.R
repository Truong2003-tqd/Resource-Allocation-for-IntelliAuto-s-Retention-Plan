library(readxl)
IntelliAuto <- read_excel("ISYS3447_ISYS3448_A3_IntelliAuto.xlsx")

#Import library
{
  library(readr)
  library(tidyverse)
  library(gtExtras)
  library(patchwork)
  library(gridExtra)
  library(skimr)
  library(corrplot)
  library(ggplot2)
  library(grid)
  library(broom)
  library(car)
}

#Create working dataframe
df <- IntelliAuto

#Glimpse data
glimpse(df)

#Skim data
skim(df) 

#Check duplication
sum(duplicated(df))

#Descriptive Summary
summary(df)

#Occupation summary table
df %>% 
  group_by(Occupation) %>% 
  summarise(Number = n()) %>% 
  arrange(desc(Number))

#Convert to factor
df <- df %>% 
  mutate(Sex = factor(Sex, levels = c("Male", "Female")),
         Union_Member = factor(Union_Member, levels = c("Yes", "No")),
         Future_Promotion = factor(Future_Promotion, levels = c("Very Unlikely", "Unlikely", "Not sure", "Likely", "Very Likely")),
         Sex_Promotion = factor(Sex_Promotion, levels = c ("Better", "Worse", "No Effect")),
         Aware = factor(Aware, levels = c("Yes", "No")))
str(df)

#Set up color code
major_color <- "#0F4761"
minor_color <- "#B02133"
