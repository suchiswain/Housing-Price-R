
library("dplyr")
library("tidyverse")
library("readxl")
##install.packages("rlang")
##install.packages("ggplot2")
library(ggplot2)


Housing_Values <- read_excel("Housing_Data_All_Predictors.xlsx")
summary(Housing_Values)

Housing_Values_1<-Housing_Values %>% 
 select(-'Residential Population',-'Per Capita Income')
Housing_Values_1 <- Housing_Values_1 %>%
  filter(`Region_Name` %in% c("Boston, MA","New York, NY","Los Angeles, CA","Raleigh, NC",
                              "San Francisco, CA","Chicago, IL")) %>%
  filter(Inventory != "N/A") %>%
  filter( Median_House_size != "N/A") %>%
 drop_na() 


names(Housing_Values_1)

Housing_Values_2 <- separate(Housing_Values_1, 'Region_Name', into = c("city", "state"), sep = ",") %>% mutate(city = str_trim(city), state= str_trim(state))

Housing_Values_Raleigh<- Housing_Values_2 %>% filter(city=='Raleigh')

Housing_Values_Raleigh <- Housing_Values_Raleigh %>% 
  mutate(log_Price= log(House_Value_Index))  


Housing_Values_Raleigh <- Housing_Values_Raleigh %>% 
  mutate(log_Mortgage= log(Mortgage_Rate)) 

##hist(Housing_Values_2$House_Value_Index)

ggplot(Housing_Values_2, aes(x=city))+  geom_bar()

ggplot(Housing_Values_2, aes(x=House_Value_Index ,y=Mortgage_Rate))+  geom_line()

ggplot(Housing_Values_Raleigh, aes(x=House_Value_Index ,y=Inflation_Rate))+  geom_line()

ggplot(Housing_Values_Raleigh, aes(x=House_Value_Index ,y=Inventory))+  geom_line()

ggplot(Housing_Values_Raleigh, aes(x=House_Value_Index ,y=Median_House_size))+  geom_line()

