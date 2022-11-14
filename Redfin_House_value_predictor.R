library("dplyr")
library("tidyverse")
library("readxl")
##install.packages("rlang")
##install.packages("ggplot2")
library(ggplot2)
library(ggpubr)

## Read the dataframe from excel file
House_Values <- read.csv("realtor-data.csv")
summary(House_Values)
House_Values$sold_date <- as.Date(House_Values$sold_date)
str(House_Values)
summary(House_Values)
colSums(is.na(House_Values))

duplicated(House_Values)
House_Values_2 <- House_Values[!duplicated(House_Values), ]
summary(House_Values_2)

House_Values_2<- House_Values_2 %>% filter(!is.na(house_size)) %>%
  filter(!is.na(bed)) %>% filter(!is.na(bath))


colSums(is.na(House_Values_2))

House_Values_2<- House_Values_2 %>% 
  filter(!is.na(city)) %>% filter(!is.na(zip_code)) %>% 
  drop_na()

House_Values_2<- House_Values_2 %>% 
  filter(!is.na(sold_date)) %>% filter(!is.na(acre_lot)) %>% 
  drop_na()

summary(House_Values_2)


Housing_Values_Sort <- House_Values_2[order(-House_Values_2$price),] 

Housing_Values_Sort <- House_Values_2[order(-House_Values_2$house_size),] 

#Filter houses with less than 3 million to remove the outliers
House_Values_2<- House_Values_2 %>% filter(price<30000000)
summary(House_Values_2)

#Filter houses with less than 20 beds
House_Values_2<- House_Values_2 %>% filter(bed<20)
summary(House_Values_2)

#Filter houses with less than 20 beds
House_Values_2<- House_Values_2 %>% filter(bath<15)
summary(House_Values_2)

ggplot(House_Values_2, aes(x=sold_date ,y=price))+  geom_line() + geom_smooth(method="lm", se = FALSE)

ggplot(House_Values_2, aes(x=bath ,y=price))+  geom_point() + geom_smooth(method="lm", se = FALSE)

ggplot(House_Values_2, aes(x=bed ,y=price))+  geom_point() + geom_smooth(method="lm", se = FALSE)

ggplot(House_Values_2, aes(x=house_size ,y=price))+  geom_point() + geom_smooth(method="lm", se = FALSE)

ggplot(House_Values_2, aes(x=acre_lot ,y=price))+  geom_point() + geom_smooth(method="lm", se = FALSE)

ggplot(House_Values_2, aes(x=state ,y=price,color=state))+  geom_point() + geom_smooth(method="lm", se = FALSE)

#Display the house value index
hist(House_Values_2$price)

# add a log value for price as a new column for more clarity 
House_Values_2 <- House_Values_2 %>% 
  mutate(log_Price= log(price))  

#Display the house value index
hist(House_Values_2$log_Price)

library(broom)

regression1 <- lm(price~bath, data = House_Values_2)
summary(regression1)
tidy(regression1)


regression2 <- lm(price ~house_size  , data = House_Values_2)
summary(regression2)
tidy(regression2)

regression3 <- lm(price ~bed  , data = House_Values_2)
summary(regression3)
tidy(regression3)

regression4 <- lm(price ~sold_date  , data = House_Values_2)
summary(regression4)

tidy(regression1) %>% filter(term=="bath") %>% 
  select(p.value)
tidy(regression1)
tidy(regression2)
tidy(regression3)
tidy(regression4)

