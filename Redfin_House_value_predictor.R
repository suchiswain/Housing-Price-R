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

FIPS_Values <- read.csv("ZIP-COUNTY-FIPS_2017-06.csv")
summary(FIPS_Values)

FIPS_Values <- FIPS_Values %>% 
  select(ZIP,STCOUNTYFP)



#Count and Display the na values in each column
colSums(is.na(House_Values))

#Remove duplicate values from the data
duplicated(House_Values)
House_Values_2 <- House_Values[!duplicated(House_Values), ]
summary(House_Values_2)


#Remove the null values from house_size,bed and bath predictors
House_Values_2<- House_Values_2 %>% filter(!is.na(house_size)) %>%
  filter(!is.na(bed)) %>% filter(!is.na(bath))

#Display the na values in the columns
colSums(is.na(House_Values_2))

#Remove na values from city and zipcode
House_Values_2<- House_Values_2 %>% 
  filter(!is.na(city)) %>% filter(!is.na(zip_code)) %>% 
  drop_na()

#Remove na values from acre_lot and sold_date
House_Values_2<- House_Values_2 %>% 
  filter(!is.na(sold_date)) %>% filter(!is.na(acre_lot)) %>% 
  drop_na()

summary(House_Values_2)



#Sort the data by price to see the highest price
Housing_Values_Sort <- House_Values_2[order(-House_Values_2$price),] 

Housing_Values_Sort <- House_Values_2[order(-House_Values_2$house_size),] 

#Filter houses with less than 3 million to remove the data inparity
House_Values_2<- House_Values_2 %>% filter(price<30000000)
summary(House_Values_2)

#Filter houses with less than 20 beds
House_Values_2<- House_Values_2 %>% filter(bed<20)
summary(House_Values_2)

#Filter houses with less than 15 baths
House_Values_2<- House_Values_2 %>% filter(bath<15)
summary(House_Values_2)

#Read the school data
Schools <- read.csv("Public_School_Data.csv")
summary(Schools)

#Count the schools by zipcode
Schools_Count<- Schools %>% count(ZIP,sort=TRUE,name = "NumOfSchools") #%>% group_by(COUNTYFIPS)
summary(Schools)

#Join the school data on zip code
# House_Values_3 <- House_Values_2 %>% inner_join(Schools_Count, by=c("zip_code"="ZIP"))
# summary(House_Values_3)

Hospitals <- read.csv("Hospital General Information.csv")
summary(Hospitals)

Hospitals_Count<- Hospitals %>% count(ZIP,sort=TRUE,name = "NumOfHospitals") #%>% group_by(COUNTYFIPS)
summary(Hospitals_Count)

#Join the school and hospital data on zip code
House_Values_3 <- House_Values_2 %>% inner_join(Schools_Count, by=c("zip_code"="ZIP")) %>% 
  inner_join(Hospitals_Count, by=c("zip_code"="ZIP"))
summary(House_Values_3)



#Plots without school data

ggplot(House_Values_2, aes(x=sold_date ,y=price))+  geom_line() + geom_smooth(method="lm", se = FALSE)

ggplot(House_Values_2, aes(x=bath ,y=price))+  geom_point() + geom_smooth(method="lm", se = FALSE)

ggplot(House_Values_2, aes(x=bed ,y=price))+  geom_point() + geom_smooth(method="lm", se = FALSE)

ggplot(House_Values_2, aes(x=house_size ,y=price))+  geom_point() + geom_smooth(method="lm", se = FALSE)

ggplot(House_Values_2, aes(x=acre_lot ,y=price))+  geom_point() + geom_smooth(method="lm", se = FALSE)

ggplot(House_Values_2, aes(x=state ,y=price,color=state))+  geom_point() + geom_smooth(method="lm", se = FALSE)


#Plot on dataframe with number of schools and hospitals

ggplot(House_Values_3, aes(x=NumOfSchools ,y=price))+  geom_point() + geom_smooth(method="lm", se = FALSE)

ggplot(House_Values_3, aes(x=NumOfHospitals ,y=price))+  geom_point() + geom_smooth(method="lm", se = FALSE)

#Display the house value index
hist(House_Values_2$price)

# add a log value for price as a new column for more clarity 
House_Values_2 <- House_Values_2 %>% 
  mutate(log_Price= log(price))  

#Display the house value index
hist(House_Values_2$log_Price)

library(broom)

regression1 <- lm(price~bath, data = House_Values_3)
summary(regression1)
tidy(regression1)


regression2 <- lm(price ~house_size  , data = House_Values_3)
summary(regression2)
tidy(regression2)

regression3 <- lm(price ~bed  , data = House_Values_3)
summary(regression3)
tidy(regression3)

regression4 <- lm(price ~sold_date  , data = House_Values_3)
summary(regression4)

regression5 <- lm(price ~acre_lot  , data = House_Values_2)
summary(regression5)

regression6 <- lm(price ~city  , data = House_Values_2)
summary(regression6)

regression7 <- lm(price ~NumOfSchools  , data = House_Values_3)
summary(regression7)

regression8 <- lm(price ~NumOfHospitals  , data = House_Values_3)
summary(regression8)


tidy(regression1)
tidy(regression2)
tidy(regression3)
tidy(regression4)
tidy(regression5)
tidy(regression8)
tidy(regression7)

