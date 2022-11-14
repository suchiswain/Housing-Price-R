
library("dplyr")
library("tidyverse")
library("readxl")
##install.packages("rlang")
##install.packages("ggplot2")
library(ggplot2)
library(ggpubr)
library(broom)

## Read the dataframe from excel file
New_Housing_Values <- read_excel("Housing_Data_All_Predictors.xlsx")
summary(New_Housing_Values)

New_Housing_Values<-New_Housing_Values %>% 
  select(-Residential_Population,-Per_Capita_Income,-Active_Listings,-Per_Capita_Income,-Inventory,-Median_House_size) 
  
  New_Housing_Values<-New_Housing_Values %>%drop_na()

summary(New_Housing_Values)

New_Housing_Values <- New_Housing_Values %>% 
  mutate(log_Price= log(House_Value_Index)) 

New_Housing_Values <- New_Housing_Values %>% 
  mutate(log_Mortgage= log(Mortgage_Rate)) 

New_Housing_Values <- New_Housing_Values %>% 
  mutate(log_Inflation= log(Inflation_Rate)) 
summary(New_Housing_Values)

p1<- ggplot(New_Housing_Values, aes(x=Mortgage_Rate ,y=log_Price))+  geom_point() + geom_smooth(method="lm", se = FALSE)



p3<-ggplot(New_Housing_Values, aes(x=Inflation_Rate ,y=log_Price))+  geom_point() + geom_smooth(method="lm", se = FALSE)

regressionmodel1 <- lm(log_Price ~ log_Mortgage +log_Inflation , data = New_Housing_Values)
summary(regressionmodel1)
tidy(regressionmodel1)

regressionmodel2 <- lm(log_Price ~ log_Mortgage , data = New_Housing_Values)
summary(regressionmodel2)
tidy(regressionmodel2)