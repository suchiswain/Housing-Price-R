library("dplyr")
library("tidyverse")
library("readxl")
##install.packages("rlang")
##install.packages("ggplot2")
library(ggplot2)
library(ggpubr)
library(lubridate)

## Read the dataframe from excel file
House_Values <- read.csv("realtor-data.csv")
summary(House_Values)
House_Values$sold_date <- as.Date(House_Values$sold_date)
str(House_Values)
summary(House_Values)

# FIPS_Values <- read.csv("ZIP-COUNTY-FIPS_2017-06.csv")
# summary(FIPS_Values)
# 
# FIPS_Values <- FIPS_Values %>% 
#   select(ZIP,STCOUNTYFP)



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

#Filter houses with less than 3 million to remove the data outliers
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

#converting sold date to the first date of the month in to the new column 
House_Values_3 <- House_Values_3 %>%
  mutate(new_date=lubridate::floor_date(sold_date, unit = "month"))


# Sucharitha' Code for Mortgage merge
#converting the column to date format
House_Values_3$new_date <- as.Date( House_Values_3$new_date )
str(House_Values_3)

#Reading the Mortgage date
mortgage_rate <- read_excel("MORTGAGE30US.xls")
summary(mortgage_rate)

#converting the column to date format
mortgage_rate$observation_date <- as.Date(mortgage_rate$observation_date)
str(mortgage_rate)
summary(mortgage_rate)

#Join mortgage rate and House value data
House_Values_4 <- House_Values_3 %>% 
  inner_join(mortgage_rate,by=c("new_date"="observation_date"))
summary(House_Values_4)

# Converting mortgage to log value

House_Values_4 <- House_Values_4 %>%
  mutate(log_mortg = log(MORTGAGE30US))

summary( House_Values_4)



#Display the house value index


h <- hist(House_Values_4$price, plot=FALSE)
cuts <- cut(h$breaks, c(-Inf,-.5,1.75,Inf))
plot(h, col=cuts)
# add a log value for price as a new column for more clarity 
House_Values_4 <- House_Values_4 %>% 
  mutate(log_Price= log(price))  

#Display the house value index

h <- hist(House_Values_4$log_Price, plot=FALSE)
cuts <- cut(h$breaks, c(-Inf,-.5,1.75,Inf))
plot(h, col=cuts)



House_Values_plot <- House_Values_4 %>%
  filter(state %in% c("New York","Massachusetts","New Hampshire"))


#plot on dataframe with All Predictors

p1<-ggplot( House_Values_plot, aes(x=log_mortg ,y=log_Price,color=state ) )  + 
  geom_point() + geom_smooth(method="lm", se = TRUE ,color="black")+
  facet_grid(.~state)+
  labs(title = " Mortgage Rate Vs Price", x="Mortgage Rate", y="Price(log Price)")+
  theme(legend.position="none") +
  xlim(c(1, 2.5))+
  scale_colour_brewer(palette = "Set1")

#From the plots it is observed that Interest Rates don't affect house prices. Infact, it tends to go up


p2<-  ggplot(House_Values_plot, aes(x=sold_date ,y=log_Price,color=state))+ 
  geom_point() + geom_smooth(method="lm", se = TRUE,color="black")+
  facet_grid(.~state)+
  labs(title = " Sold Date Vs Price", x=" Sold Date", y="Price(log Price)")+
  theme(legend.position="none") +
  scale_colour_brewer(palette = "Set1")

#From the plots it is observed that the price keeps increasing over the period

p3<- ggplot(House_Values_plot, aes(x=bath ,y=log_Price,color=state))+  
  geom_point() + geom_smooth(method="lm", se = TRUE,color="black") +
  facet_grid(.~state)+
  labs(title = " No. of Bathrooms Vs Price", x=" BathRooms", y="Price(log Price)")+
  theme(legend.position="none") +
  xlim(c(1, 6))+
  scale_colour_brewer(palette = "Set1")

# Observed that bathrooms increases the value of House prices


p4<- ggplot(House_Values_plot, aes(x=bed ,y=log_Price,color=state))+  
  geom_point() + geom_smooth(method="lm", se = TRUE,color="black")+
  facet_grid(.~state)+
  labs(title = " No. of Bedrooms Vs Price", x="BedRooms", y="Price(log Price)")+
  theme(legend.position="none")  +
  xlim(c(1, 6))+
  scale_colour_brewer(palette = "Set1")
#From the plots it is observed that bedrooms increases the value of House prices



p5<- ggplot(House_Values_plot, aes(x=house_size ,y=log_Price, color=state))+  
  geom_point() + geom_smooth(method="lm", se = TRUE,color="black")+
  facet_grid(.~state)+
  labs(title = " House Size Vs Price", x="House Size", y="Price(log Price)")+
  theme(legend.position="none") +
  xlim(c(1000, 5000))+
  scale_colour_brewer(palette = "Set1")

#Greater the house size, the greater the home value




p6<- ggplot(House_Values_plot, aes(x=acre_lot ,y=log_Price,color=state))+  
  geom_point() + geom_smooth(method="lm", se = TRUE,color="black")+
  facet_grid(.~state)+
  labs(title = " Acre Lot Vs Price", x="Acre Lot", y="Price(log Price)")+
  xlim(c(0, 5))+
  theme(legend.position="none") +
  scale_colour_brewer(palette = "Set1")
#observed that larger lots have a higher property value



p7<- ggplot(House_Values_plot, aes(x=NumOfHospitals ,y=log_Price,color=state))+ 
  geom_point() + geom_smooth(method="lm", se = TRUE,color="black") +
  facet_grid(.~state)+
  labs(title = " No. of Hospitals Vs Price", x="No. of Hospitals", y="Price(log Price)")+
  theme(legend.position="none") +
  scale_colour_brewer(palette = "Set1")
#Hospitals clearly have an impact on house prices


#No of schools increases property value. From the plot, Massachusetts prices dropped but overall the price increases
p8<- ggplot(House_Values_plot, aes(x= NumOfSchools ,y=log_Price,color=state))+ 
  geom_point() + geom_smooth(method="lm", se = TRUE,color="black") +
  facet_grid(.~state)+
  labs(title = " No. of schools Vs Price", x="No. of Schools", y="Price(log Price)")+
  theme(legend.position="none") +
  xlim(c(0, 20))+
  scale_colour_brewer(palette = "Set1")


# Plot grid to show all the predictor plots

ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,
          labels = c("A", "B", "C","D","E","F","G","H"),
          ncol = 3, nrow = 3)



# Run regression all the predictors individually to get the significant predictors( P value)
# P value less than 0.05 is statistically significant
library(broom)

regression1 <- lm(log_Price~bath, data = House_Values_plot)
summary(regression1)
tidy(regression1)


regression2 <- lm(log_Price ~house_size  , data = House_Values_plot)
summary(regression2)
tidy(regression2)

regression3 <- lm(log_Price ~bed  , data = House_Values_plot)
summary(regression3)
tidy(regression3)

regression4 <- lm(log_Price ~sold_date  , data = House_Values_plot)
summary(regression4)

regression5 <- lm(log_Price ~acre_lot  , data = House_Values_plot)
summary(regression5)

regression7 <- lm(log_Price ~NumOfSchools  , data = House_Values_plot)
summary(regression7)

regression8 <- lm(log_Price ~NumOfHospitals  , data = House_Values_plot)
summary(regression8)



tidy(regression1)
tidy(regression2)
tidy(regression3)
tidy(regression4)
tidy(regression5)
tidy(regression8)
tidy(regression7)


#Develop Model
# Create few regression model to compare them and select the best that fits. Compare R-squared
# R-squared: represents the proportion of variance explained by the model. The larger the values, the greater the model fit.


#Multiple R-squared:  0.4268,	Adjusted R-squared:  0.4241
regressionmodel1 <- lm(log_Price ~ bath + house_size + bed + NumOfHospitals + NumOfSchools +log_mortg + acre_lot +sold_date  , data = House_Values_plot)
summary(regressionmodel1)
tidy(regressionmodel1)

#regressionmodel2=  Multiple R-squared:  0.4107,	Adjusted R-squared:  0.4087 (excluded NumOfHospitals+NumOfSchools)

regressionmodel2 <- lm(log_Price ~ bath + house_size + bed +log_mortg + acre_lot +sold_date  , data = House_Values_plot)
summary(regressionmodel2)
tidy(regressionmodel2)


#regressionmodel3 =Multiple R-squared:  0.4107,	Adjusted R-squared:  0.409   0.3744 (excluded NumOfHospitals+NumOfSchools +sold_date)

regressionmodel3 <- lm(log_Price ~ bath + house_size + bed +log_mortg + acre_lot, data = House_Values_plot)
summary(regressionmodel3)
tidy(regressionmodel3)

#Multiple R-squared:  0.4267,	Adjusted R-squared:  0.4244 
regressionmodel4 <- lm(log_Price ~ bath + house_size + bed + NumOfHospitals + NumOfSchools +log_mortg + acre_lot, data = House_Values_plot)
summary(regressionmodel4)
tidy(regressionmodel4)

#Multiple R-squared:  0.4262,	Adjusted R-squared:  0.4242 
regressionmodel5 <- lm(log_Price ~ bath + house_size + bed + NumOfHospitals + NumOfSchools + acre_lot, data = House_Values_plot)
summary(regressionmodel5)
tidy(regressionmodel5)

#Multiple R-squared:  0.4101,	Adjusted R-squared:  0.4087
regressionmodel6 <- lm(log_Price ~ bath + house_size + bed + acre_lot, data = House_Values_plot)
summary(regressionmodel6)
tidy(regressionmodel6)

#Multiple R-squared:  0.4268,	Adjusted R-squared:  0.4245
regressionmodel7 <- lm(log_Price ~ bath + house_size + bed + NumOfHospitals + NumOfSchools + acre_lot +sold_date  , data = House_Values_plot)
summary(regressionmodel7)
tidy(regressionmodel7)





#Train and Test The model
# Train and Test 
## Calculate RMSE on test data
## A metric that tells us how far apart the predicted values are from the observed values in a dataset, on average. 
## The lower the RMSE, the better a model fits a dataset.

#Train and Test The model
# Train and Test 

#libraries Required
library(rsample)
library(yardstick)

#Code goes here
set.seed(645)
House_Values_plot_split <- initial_split(House_Values_plot, prop = 0.7)
House_Values_plot_train <- training(House_Values_plot_split)
House_Values_plot_test <- testing(House_Values_plot_split)
House_Values_plot_train


#Using regressionmodel4 to evaluate the data

# Evaluation on train data
regression_train  <- lm(log_Price ~ bath + house_size + bed + NumOfHospitals + 
                          NumOfSchools +log_mortg + acre_lot   , data = House_Values_plot_train)

House_Values_plot_train <- House_Values_plot_train %>% 
  mutate(predicted_House_Values_plot= predict(regression_train ))
rmse(House_Values_plot_train, log_Price, predicted_House_Values_plot)

# rmse    standard    0.654

# Evaluation on test data
House_Values_plot_test  <- House_Values_plot_test  %>% 
  mutate(predicted_House_Values_plot = predict(regression_train, newdata = House_Values_plot_test))
rmse(House_Values_plot_test, log_Price, predicted_House_Values_plot)

#rmse    standard    0.610

#Using regressionmodel4 to evaluate the data
#NEW Evaluation on train data
regression_train  <- lm(log_Price ~ bath + house_size + bed + 
                          NumOfHospitals + NumOfSchools + acre_lot +sold_date, data = House_Values_plot_train)

House_Values_plot_train <- House_Values_plot_train %>% 
  mutate(predicted_House_Values_plot= predict(regression_train ))
rmse(House_Values_plot_train, log_Price, predicted_House_Values_plot)

# rmse    standard    0.654

# Evaluation on test data
House_Values_plot_test  <- House_Values_plot_test  %>% 
  mutate(predicted_House_Values_plot = predict(regression_train, newdata = House_Values_plot_test))
rmse(House_Values_plot_test, log_Price, predicted_House_Values_plot)

#rmse    standard    0.609

#NEW Evaluation on train data
regression_train  <- lm(log_Price ~ bath + house_size + bed + 
                          NumOfHospitals + NumOfSchools + acre_lot +sold_date, data = House_Values_plot_train)

House_Values_plot_train <- House_Values_plot_train %>% 
  mutate(predicted_House_Values_plot= predict(regression_train ))
rmse(House_Values_plot_train, log_Price, predicted_House_Values_plot)

# rmse    standard    0.654

# Evaluation on test data
House_Values_plot_test  <- House_Values_plot_test  %>% 
  mutate(predicted_House_Values_plot = predict(regression_train, newdata = House_Values_plot_test))
rmse(House_Values_plot_test, log_Price, predicted_House_Values_plot)

#rmse    standard    0.609

# Logistic Regression MODEL - Higher the AUC better the model

library(caret)
library(cutpointr)

# We have selected "log_Price" to be the binary variable
#Create new variable price_binary defined as 1 if percentage > 14 and 0 otherwise

set.seed(645)

House_Values_plot <- House_Values_plot %>%
  mutate(price_binary = if_else(log_Price>14, 1, 0))
summary(House_Values_plot)
# Train and Test the logistic model1

House_Values_plot_split1 <- initial_split(House_Values_plot, prop = 0.7)
House_Values_plot_train1 <- training(House_Values_plot_split1)
House_Values_plot_test1 <- testing(House_Values_plot_split1)

str(House_Values_plot_train1)

# Logistic Model 1 with all the predictor variables- 
price_binary1 <- glm(price_binary ~ bed + bath + acre_lot + house_size + NumOfSchools + NumOfHospitals + log_mortg + sold_date, data = House_Values_plot_train1, family = "binomial")
summary(price_binary1) 

# ROC Curve and AUC
# AUC is 0.9292294
House_Values_plot_test1 <- House_Values_plot_test1 %>% 
  mutate(predicted_probability_default = predict(price_binary1, newdata = House_Values_plot_test1, type = "response")) 

roc_1 <- roc(House_Values_plot_test1, x= predicted_probability_default, class = price_binary, pos_class = 1, neg_class = 0)

plot(roc_1)
auc(roc_1)
plot(roc_1) + 
  geom_line(data = roc_1, color = "red") + 
  geom_abline(slope = 1) + 
  labs(title = "ROC Curve for Logistic Regression")

# Logistic Model 2 with all the predictor variables but excluding NumOfSchools and NumOfHospitals 
price_binary2 <- glm(price_binary ~ bed + bath + acre_lot + house_size + sold_date + log_mortg, data = House_Values_plot_train1, family = "binomial")
summary(price_binary2) 

# ROC Curve and AUC
# AUC is 0.9311881
House_Values_plot_test1 <- House_Values_plot_test1 %>% 
  mutate(predicted_probability_default = predict(price_binary2, newdata = House_Values_plot_test1, type = "response")) 

roc_2 <- roc(House_Values_plot_test1, x= predicted_probability_default, class = price_binary, pos_class = 1, neg_class = 0)

plot(roc_2)
auc(roc_2)
plot(roc_2) + 
  geom_line(data = roc_2, color = "red") + 
  geom_abline(slope = 1) + 
  labs(title = "ROC Curve for Logistic Regression")

# Logistic Model 3 with all the predictor variables but excluding bed, bath, and sold_date
price_binary3 <- glm(price_binary ~ acre_lot + house_size + NumOfSchools + NumOfHospitals + log_mortg , data = House_Values_plot_train1, family = "binomial")
summary(price_binary3) 

# ROC Curve and AUC
# AUC is 0.5073612
House_Values_plot_test1 <- House_Values_plot_test1 %>% 
  mutate(predicted_probability_default = predict(price_binary3, newdata = House_Values_plot_test1, type = "response")) 

roc_3 <- roc(House_Values_plot_test1, x= predicted_probability_default, class = price_binary, pos_class = 1, neg_class = 0)

plot(roc_3)
auc(roc_3)
plot(roc_3) + 
  geom_line(data = roc_3, color = "red") + 
  geom_abline(slope = 1) + 
  labs(title = "ROC Curve for Logistic Regression")

# Logistic Model 4 with all the predictor variables but excluding acre_lot and house_size 
price_binary4 <- glm(price_binary ~ bed + bath + NumOfSchools + NumOfHospitals + sold_date + log_mortg, data = House_Values_plot_train1, family = "binomial")
summary(price_binary4) 

# ROC Curve and AUC
# AUC is 0.9292079
House_Values_plot_test1 <- House_Values_plot_test1 %>% 
  mutate(predicted_probability_default = predict(price_binary4, newdata = House_Values_plot_test1, type = "response")) 

roc_4 <- roc(House_Values_plot_test1, x= predicted_probability_default, class = price_binary, pos_class = 1, neg_class = 0)

plot(roc_4)
auc(roc_4)
plot(roc_4) + 
  geom_line(data = roc_4, color = "red") + 
  geom_abline(slope = 1) + 
  labs(title = "ROC Curve for Logistic Regression")

# Logistic Model 5 with all the predictor variables but excluding  sold_date
price_binary5 <- glm(price_binary ~ bed + bath + acre_lot + house_size + NumOfSchools + NumOfHospitals + log_mortg , data = House_Values_plot_train1, family = "binomial")
summary(price_binary5) 

# ROC Curve and AUC
# AUC is 0.5049935
House_Values_plot_test1 <- House_Values_plot_test1 %>% 
  mutate(predicted_probability_default = predict(price_binary5, newdata = House_Values_plot_test1, type = "response")) 

roc_5 <- roc(House_Values_plot_test1, x= predicted_probability_default, class = price_binary, pos_class = 1, neg_class = 0)

plot(roc_5)
auc(roc_5)
plot(roc_5) + 
  geom_line(data = roc_5, color = "red") + 
  geom_abline(slope = 1) + 
  labs(title = "ROC Curve for Logistic Regression")

# Logistic Model 5 with all the predictor variables but excluding  log_mrtg
price_binary6 <- glm(price_binary ~ bed + bath + acre_lot + house_size + NumOfSchools + NumOfHospitals +sold_date , data = House_Values_plot_train1, family = "binomial")
summary(price_binary6) 

# ROC Curve and AUC
# AUC is 0.5049935
House_Values_plot_test1 <- House_Values_plot_test1 %>% 
  mutate(predicted_probability_default = predict(price_binary6, newdata = House_Values_plot_test1, type = "response")) 

roc_6 <- roc(House_Values_plot_test1, x= predicted_probability_default, class = price_binary, pos_class = 1, neg_class = 0)

plot(roc_6)
auc(roc_6)
plot(roc_6) + 
  geom_line(data = roc_6, color = "red") + 
  geom_abline(slope = 1) + 
  labs(title = "ROC Curve for Logistic Regression")

auc(roc_1)
auc(roc_2)
auc(roc_3)
auc(roc_4)
auc(roc_5)
auc(roc_6)


#Logistic Model 6 is a good fit as it has highest AUC (AUC 0.8523758) 


#Confusion Matrix

House_Values_plot_test1 <- House_Values_plot_test1 %>% 
  mutate(predicted_log_price = predict(price_binary6, newdata = House_Values_plot_test1, type = "response")) %>%
  mutate(predicted_log_price1 = if_else(predicted_log_price>0.5, 1, 0)) %>% 
  mutate(predicted_log_price2 = if_else(predicted_log_price>0.2, 1, 0)) %>% 
  mutate(predicted_log_price3 = if_else(predicted_log_price>0.3, 1, 0)) %>% 
  mutate(predicted_log_price4 = if_else(predicted_log_price>0.4, 1, 0)) %>% 
  mutate(predicted_log_price5 = if_else(predicted_log_price>0.1, 1, 0))
  
  

cm1<-confusionMatrix(as.factor(House_Values_plot_test1$predicted_log_price1), as.factor(House_Values_plot_test1$price_binary), positive = "1")

cm2<-confusionMatrix(as.factor(House_Values_plot_test1$predicted_log_price2), as.factor(House_Values_plot_test1$price_binary), positive = "1")

cm3<-confusionMatrix(as.factor(House_Values_plot_test1$predicted_log_price3), as.factor(House_Values_plot_test1$price_binary), positive = "1")

cm4<-confusionMatrix(as.factor(House_Values_plot_test1$predicted_log_price4), as.factor(House_Values_plot_test1$price_binary), positive = "1")

cm5<-confusionMatrix(as.factor(House_Values_plot_test1$predicted_log_price5), as.factor(House_Values_plot_test1$price_binary), positive = "1")

tidy(cm1)
tidy(cm2)
tidy(cm3)
tidy(cm4)
tidy(cm5)
# confusion matrix 3 has the best cutoff

# Determining optimal cutoff to maximize accuracy or the sum of sensitivity and specificity

cpt1<- cutpointr(House_Values_plot_test1, x= predicted_log_price, class = price_binary, pos_class = 1, neg_class = 0, 
          method = maximize_metric, metric = accuracy, na.rm = TRUE)

cpt2<-cutpointr(House_Values_plot_test1, x= predicted_log_price, class = price_binary, pos_class = 1, neg_class = 0, 
          method = maximize_metric, metric = sum_sens_spec, na.rm = TRUE)

# the optimal cutoff value of 0.2235809 in cpt2 
House_Values_plot_test1 <- House_Values_plot_test1 %>% 
  mutate(predicted_log_price = predict(Mortgage_percentage2, newdata = House_Values_plot_test1, type = "response")) %>%
  mutate(predicted_mortgage1 = if_else(predicted_mortgage_rate>0.269, 1, 0))

# Using the optimal cutoff for the confusion matrix.
confusionMatrix(as.factor(House_Values_plot_test1$predicted_log_price3), as.factor(House_Values_plot_test1$price_binary), positive = "1")



#decision tree model1

library(rpart)
library(rpart.plot)
model1 <- rpart(log_Price ~ bath + house_size + bed + NumOfHospitals + 
                  NumOfSchools +log_mortg + acre_lot   , data = House_Values_plot_train, method ="class")
model1.plot <- rpart.plot(model1,box.palette = "blue", cex=0.8)
summary(model1)

model2 <- rpart(log_Price ~ bath + house_size + bed + 
                  NumOfHospitals + NumOfSchools + acre_lot + sold_date, data = House_Values_plot_train, method="class")
model2.plot <- rpart.plot(model2,box.palette = "yellow", cex=0.8)
summary(model2)

model3 <- rpart(log_Price ~ bath + house_size + bed + 
                  NumOfHospitals + NumOfSchools + acre_lot , data = House_Values_plot_train, method ="class")
model3.plot <- rpart.plot(model3,box.palette = "green", cex=0.8)
summary(model3)


