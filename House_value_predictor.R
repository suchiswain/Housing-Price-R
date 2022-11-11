
library("dplyr")
library("tidyverse")
library("readxl")
##install.packages("rlang")
##install.packages("ggplot2")
library(ggplot2)
library(ggpubr)

## Read the dataframe from excel file
Housing_Values <- read_excel("Housing_Data_All_Predictors.xlsx")
summary(Housing_Values)

#Remove the annual predictor data from the dataframe

Housing_Values_1<-Housing_Values %>% 
 select(-'Residential Population',-'Per Capita Income')

# Filter the data msa wise
Housing_Values_2 <- Housing_Values_1 %>%
  filter(`Region_Name` %in% c("Boston, MA","New York, NY","Los Angeles, CA","Raleigh, NC",
                              "San Francisco, CA","Chicago, IL")) %>%
  filter(Inventory != "N/A") %>%
  filter( Median_House_size != "N/A") %>%
 drop_na() 



names(Housing_Values_1)

# Separate the region name into city and state to add 2 extra columns
Housing_Values_3 <- separate(Housing_Values_2, 'Region_Name', into = c("city", "state"), sep = ",") %>%
  mutate(city = str_trim(city), state= str_trim(state))

Housing_Values_3 <- Housing_Values_3 %>% 
  mutate(log_Price= log(House_Value_Index))  

# Housing_Values_Raleigh <- Housing_Values_Raleigh %>% 
#   mutate(log_Mortgage= log(Mortgage_Rate)) 

#Plots for all msa's for each predictor

p1<- ggplot(Housing_Values_3, aes(x=House_Value_Index ,y=Mortgage_Rate,color=city))+  geom_point() + geom_smooth(method="lm", se = FALSE)

p2<-ggplot(Housing_Values_3, aes(x=House_Value_Index ,y=Inventory,color=city))+  geom_point() + geom_smooth(method="lm", se = FALSE)

p3<-ggplot(Housing_Values_3, aes(x=House_Value_Index ,y=Inflation_Rate,color=city))+  geom_point() + geom_smooth(method="lm", se = FALSE)

p4<-ggplot(Housing_Values_3, aes(x=House_Value_Index ,y=Median_House_size,color=city))+  geom_point() + geom_smooth(method="lm", se = FALSE)


ggarrange(p1,p2,p3,p4,
          labels = c("A", "B", "C","D"),
          ncol = 1, nrow = 4)

# Filter data for Raleigh msa
Housing_Values_Raleigh<- Housing_Values_3 %>% filter(city=='Raleigh')

hist(Housing_Values_Raleigh$log_Price)

ggplot(Housing_Values_Raleigh, aes(x=log_Price))+  geom_bar()

ggplot(Housing_Values_Raleigh, aes(x=House_Value_Index ,y=Inflation_Rate))+  geom_line()

ggplot(Housing_Values_Raleigh, aes(x=House_Value_Index ,y=Inventory))+  geom_line()

ggplot(Housing_Values_Raleigh, aes(x=House_Value_Index ,y=Median_House_size))+  geom_line()

