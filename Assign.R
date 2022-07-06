head(Car_sale_ads)
str(Car_sale_ads)
(colSums(is.na(Car_sale_ads))/208304)*100
cleanData = subset(Car_sale_ads, select = -c(Vehicle_version,Vehicle_generation,Origin_country,CO2_emissions,First_owner,First_registration_date) )
head(cleanData)
for(i in 7:10)
{                                   
  cleanData[ , i][is.na(cleanData[ , i])] <- mean(cleanData[ , i], na.rm = TRUE)
}
cleanData=na.omit(cleanData)
(colSums(is.na(cleanData))/208304)*100

cleanData$Price= ifelse(cleanData$Currency == 'EUR',cleanData$Price*4.5, cleanData$Price)
cleanData = subset(cleanData, select = -c(Currency))

cleanData$Doors_number<-factor(cleanData$Doors_number)

cleanData$Offer_publication_date<-as.Date(cleanData$Offer_publication_date,"%d/%m/%Y")

cleanData = subset(cleanData, select = -c(Offer_location, Features))

cleanData = subset(cleanData, select = -c(Index))

summary(Filter(is.numeric, cleanData))

summary(Filter(is.factor, cleanData))

levels(cleanData$Vehicle_brand)

cleanData = subset(cleanData, select = -c(Vehicle_brand))

levels(cleanData$Vehicle_model)

cleanData = subset(cleanData, select = -c(Vehicle_model))

plot(cleanData$Price, cleanData$Production_year, main="Scatterplot 1",
     xlab="Price ", ylab="Production Year ", pch=19)

cleanData = subset(cleanData, select = -c(Production_year))

plot(cleanData$Price, cleanData$Mileage_km, main="Scatterplot 2",
     xlab="Price ", ylab="Mileage ", pch=19)

plot(cleanData$Price, cleanData$Power_HP, main="Scatterplot 3",
     xlab="Price ", ylab="Power ", pch=19)

plot(cleanData$Price, cleanData$Displacement_cm3, main="Scatterplot 4",
     xlab="Price ", ylab="Displacement ", pch=19)

library(ggplot2)
ggplot(aes(x= Condition,y=Price,fill=Condition), data = cleanData) + geom_boxplot() +  xlab("Condition") + ylab("Price")+ ggtitle("Condition vs Price")

ggplot(aes(x= Fuel_type,y=Price,fill=Fuel_type), data = cleanData) + geom_boxplot() +  xlab("Fuel Type") + ylab("Price")+ ggtitle("Fuel Type vs Price")

ggplot(aes(x= Drive,y=Price,fill=Drive), data = cleanData) + geom_boxplot() +  xlab("Drive") + ylab("Price")+ ggtitle("Drive vs Price")

ggplot(aes(x= Transmission,y=Price,fill=Transmission), data = cleanData) + geom_boxplot() +  xlab("Transmission") + ylab("Price")+ ggtitle("Transmission vs Price")

ggplot(aes(x= Type,y=Price,fill=Type), data = cleanData) + geom_boxplot() +  xlab("Type") + ylab("Price")+ ggtitle("Type vs Price")

ggplot(aes(x= Doors_number,y=Price,fill=Doors_number), data = cleanData) + geom_boxplot() +  xlab("Doors") + ylab("Price")+ ggtitle("Doors vs Price")

ggplot(aes(x= Colour,y=Price,fill=Colour), data = cleanData) + geom_boxplot() +  xlab("Colour") + ylab("Price")+ ggtitle("Colour vs Price")

cleanData$Price<- log(cleanData$Price)
cleanData$Mileage_km <-log(cleanData$Mileage_km)
cleanData$Power_HP<- log(cleanData$Power_HP)
cleanData$Displacement_cm3<- log(cleanData$Displacement_cm3)

cleanData = subset(cleanData, select = -c(Offer_publication_date))

library(caret)
car_dummy <- dummyVars(" ~ .", data = cleanData, fullRank = T)
cleanData <- data.frame(predict(car_dummy, newdata = cleanData))


set.seed(123)
trainIndex <- createDataPartition(cleanData$Price,p=0.70,list=FALSE)
train <- cleanData[trainIndex,] 
test <- cleanData[-trainIndex,] 

lm_model <- lm(Price~.,data = train)
summary(lm_model)

library(MASS)
lm_model_both <- stepAIC(lm_model,direction = "both")

library(randomForest)
randomforest_model <- randomForest(Price~.,data = train,mtry = 3,ntree = 100)

model_predict_lm <- predict.lm(lm_model,newdata =test)
model_predict_both<-predict.lm(lm_model_both,newdata =test)
model_predict_randomForest <- predict(randomforest_model,newdata =test)

paste0("Linear Regression's RMSE",": ",RMSE(model_predict_lm,test$Price)) # Linear Regression
paste0("Forward and Backwards selection model's RMSE:"," ",RMSE(model_predict_both,test$Price)) # Linear Regression with forward and backward selection model
paste0("RandomForest's RMSE :"," ",RMSE(model_predict_randomForest,test$Price)) # Random Forest
