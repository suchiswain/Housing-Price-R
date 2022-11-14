
library("dplyr")
library("tidyverse")
library("readxl")
##install.packages("rlang")
##install.packages("ggplot2")
library(ggplot2)
library(ggpubr)
library(broom)

## Read the dataframe from excel file
Housing_Values <- read_excel("Housing_Data_All_Predictors.xlsx")
summary(Housing_Values)

Housing_Values_New<-Housing_Values %>% 
  select(-Residential_Population,-Per_Capita_Income)

#Remove the annual predictor data from the dataframe

Housing_Values_1<-Housing_Values %>% 
 select(-Residential_Population,-Per_Capita_Income)

# Filter the data msa wise
Housing_Values_2 <- Housing_Values %>%
  filter(`Region_Name` %in% c("Boston, MA","New York, NY","Los Angeles, CA","Raleigh, NC",
                              "San Francisco, CA","Chicago, IL")) %>%
  filter(Inventory != "N/A") %>%
  filter( Median_House_size != "N/A") %>%
 drop_na() 



names(Housing_Values_1)

# Separate the region name into city and state to add 2 extra columns
Housing_Values_3 <- separate(Housing_Values_2, 'Region_Name', into = c("city", "state"), sep = ",") %>%
  mutate(city = str_trim(city), state= str_trim(state))


#Display the house value index
hist(Housing_Values_3$House_Value_Index)

# add a log value for price as a new column for more clarity 
Housing_Values_3 <- Housing_Values_3 %>% 
  mutate(log_Price= log(House_Value_Index))  

#Plot the log price for a more even price. 
hist(Housing_Values_3$log_Price)

# Housing_Values_Raleigh <- Housing_Values_Raleigh %>% 
#   mutate(log_Mortgage= log(Mortgage_Rate)) 

# Change categorical predictors as factors.
# Housing_Values_3$Residential_Population<- as.factor(Housing_Values_3$Residential_Population)
# Housing_Values_3$Inventory <- as.factor(Housing_Values_3$Inventory)

#Plots for all msa's for each predictor
# In geom_smooth ,method used is loess as it is recommended for data records less than 1000

ggplot(Housing_Values_3, aes(x=log_Price ,y=Mortgage_Rate)) +
  geom_point(color="red") + 
  geom_smooth(method = "lm", se=FALSE)  +
  labs(x = "House Prices", y = "Mortgage Rate", 
       title = "House prices and Mortgage Rate") +
  theme_bw() + theme(panel.border = element_blank()) +
  facet_wrap(.~city)

p1<- ggplot(Housing_Values_3, aes(x=Mortgage_Rate ,y=log_Price,color=city))+  geom_point() + geom_smooth(method="loess", se = FALSE)

p2<-ggplot(Housing_Values_3, aes(x=Inventory ,y=log_Price,color=city))+  geom_point() + geom_smooth(method="lm", se = FALSE)

p3<-ggplot(Housing_Values_3, aes(x=Inflation_Rate ,y=log_Price,color=city))+  geom_point() + geom_smooth(method="loess", se = FALSE)

p4<-ggplot(Housing_Values_3, aes(x=Median_House_size ,y=log_Price,color=city))+  geom_point() + geom_smooth(method="loess", se = FALSE)

p5<-ggplot(Housing_Values_3, aes(x=Year ,y=log_Price,color=city))+  geom_point() + geom_smooth(method="loess", se = FALSE)

p6<-ggplot(Housing_Values_3, aes(x=Active_Listings ,y=log_Price,color=city))+  geom_point() + geom_smooth(method="loess", se = FALSE)

p7<-ggplot(Housing_Values_3, aes(x=Per_Capita_Income ,y=log_Price,color=city))+  geom_point() + geom_smooth(method="loess", se = FALSE)


#Arrange all the plots together for a collated view
ggarrange(p1,p2,p3,p4,p5,p6,p7,
          labels = c("A", "B", "C","D","E","F"),
          ncol = 2, nrow = 4)


regressionmodel1 <- lm(House_Value_Index ~ Mortgage_Rate , data = Housing_Values_3)
summary(regressionmodel1)
tidy(regressionmodel1)
tidy(regressionmodel1) %>%
  filter(term=="Mortgage_Rate") %>% 
  select(estimate)

regressionmodel2 <- lm(log_Price ~ Inventory , data = Housing_Values_3)
summary(regressionmodel2)

regressionmodel3 <- lm(log_Price ~ Inflation_Rate +Mortgage_Rate , data = Housing_Values_3)
summary(regressionmodel3)
tidy(regressionmodel3)
tidy(regressionmodel3) %>%
  filter(term=="Inflation_Rate") %>% 
  select(estimate)

regressionmodel4 <- lm(log_Price ~ Median_House_size , data = Housing_Values_3)
summary(regressionmodel4)
tidy(regressionmodel4) %>% filter(term=="Median_House_size") %>% 
  select(p.value)
  

regressionmodel5 <- lm(log_Price ~ Active_Listings , data = Housing_Values_3)
summary(regressionmodel5)
tidy(regressionmodel5)

regressionmodel6 <- lm(log_Price ~ Residential_Population , data = Housing_Values_3)
summary(regressionmodel6)
tidy(regressionmodel6)


regressionmodel7 <- lm(log_Price ~ Year , data = Housing_Values_3)
summary(regressionmodel7)
tidy(regressionmodel7)

regressionmodel8 <- lm(log_Price ~ Inflation_Rate+ Median_House_size + Mortgage_Rate+ Residential_Population + Per_Capita_Income + Year, data = Housing_Values_3)
summary(regressionmodel8)
tidy(regressionmodel8)

regressionmodel9 <- lm(log_Price ~ Inflation_Rate+ Median_House_size + Mortgage_Rate+ Residential_Population + Per_Capita_Income , data = Housing_Values_3)
summary(regressionmodel9)
tidy(regressionmodel9)


#Compare the two predictors: Which one is better


# predict outcome
tidy(regressionmodel1) %>%
  filter(term=="Mortgage_Rate")

tidy(regressionmodel3) %>%
  filter(term=="Inflation_Rate")

Housing_Values_3 <- Housing_Values_3 %>% 
  mutate(predicted_price_1 = predict(regressionmodel1)) %>% 
  mutate(predicted_price_2 = predict(regressionmodel3)) 

ggplot(Housing_Values_3, aes(x=Mortgage_Rate, y=predicted_price_1)) +
  geom_point() + 
  geom_smooth(method="lm", se = FALSE)

ggplot(Housing_Values_3, aes(x=Inflation_Rate, y=predicted_price_2)) +
  geom_point() + 
  geom_smooth(method="lm", se = FALSE)

# Filter data for Raleigh msa
Housing_Values_Raleigh<- Housing_Values_3 %>% filter(city=='Raleigh')

hist(Housing_Values_Raleigh$log_Price)

ggplot(Housing_Values_Raleigh, aes(x=log_Price))+  geom_bar()

ggplot(Housing_Values_Raleigh, aes(x=House_Value_Index ,y=Inflation_Rate))+  geom_line()

ggplot(Housing_Values_Raleigh, aes(x=House_Value_Index ,y=Inventory))+  geom_line()

ggplot(Housing_Values_Raleigh, aes(x=House_Value_Index ,y=Median_House_size))+  geom_line()

