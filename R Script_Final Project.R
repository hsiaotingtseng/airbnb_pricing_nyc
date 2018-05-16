library(ggplot2)
library(plyr)
library(dplyr)


Airbnb_New_York_Data <- read.csv("/Users/Mavis/Documents/HU/Analytical Methods/s3_files/new_york/Airbnb_New_York_Data.csv")
View(Airbnb_New_York_Data)

#Part 1: Overview : Airbnb listing in New York City
#Histogram of Airbnb listing price in NYC
ggplot(data = Airbnb_New_York_Data, aes(x=price)) + geom_histogram(binwidth =.1, color="black", fill="white") + scale_x_log10() + geom_vline(aes(xintercept=mean(price, na.rm=T)), color="red", linetype="dashed", size=1) + ggtitle("Histogram for Airbnb Listing Price in NYC")  

#Basic statistical measures for Airbnb listing price in NYC
summary(Airbnb_New_York_Data$price)

#Standard Deviation for Airbnb listing price in NYC
sd(Airbnb_New_York_Data$price)

#Part 2: What is the primary factor which influences the listing price in NYC?
#1. Room Type
ggplot(data = Airbnb_New_York_Data, mapping = aes(x = room_type, y = price)) + geom_boxplot() + scale_y_log10() + stat_summary(fun.y=mean, geom="point", shape=5, size=4)+ ggtitle("Boxplot for Airbnb Listing Price in NYC By Room Type")

#Basic statistical measures for Airbnb listing price by different room types
ddply(Airbnb_New_York_Data, c("room_type"), summarise, Min=min(price), Median=median(price), Mean=mean(price), Max=max(price), STD=sd(price))

#2. Property Type
ggplot(data = Airbnb_New_York_Data, mapping = aes(x = property_type, y = price)) + geom_boxplot() + scale_y_log10() + stat_summary(fun.y=mean, geom="point", shape=5, size=4) 

#Basic statistical measures for Airbnb listing price by different property types
ddply(Airbnb_New_York_Data, c("property_type"), summarise, Min=min(price), Median=median(price), Mean=mean(price), Max=max(price), STD=sd(price))

#3. Borough
ggplot(data = Airbnb_New_York_Data, mapping = aes(x = borough, y = price)) + geom_boxplot() + scale_y_log10() + stat_summary(fun.y=mean, geom="point", shape=5, size=4) + ggtitle("Boxplot for Airbnb Listing Price in NYC By Borough")

#Basic statistical measures for Airbnb listing price by different boroughs
ddply(Airbnb_New_York_Data, c("borough"), summarise, Min=min(price), Median=median(price), Mean=mean(price), Max=max(price), STD=sd(price))

#4. Neighborhood
#Basic statistical measures for Airbnb listing price by different neighborhood
ns <- ddply(Airbnb_New_York_Data, c("neighborhood"), summarise, Min=min(price), Median=median(price), Average=mean(price), Max=max(price), STD=sd(price))
ns_sorted_by_average_price_desc <- ns[with(ns, order(-Average)), ]
print(ns_sorted_by_average_price_desc)

ns_sorted_by_average_price_asc <- ns[with(ns, order(Average)), ]
print(ns_sorted_by_average_price_asc)

#5.Distance between the listing and the Empire State Building(the highest building in NYC) v.s.price
ggplot(Airbnb_New_York_Data, aes(x=distance_to._ESB, y=price)) +
  geom_point(shape=1) + geom_smooth(method=lm, se=FALSE) + ggtitle("Scatterplot between Airbnb listing price and distance between listing and the Empire State Building")
#Calculate Correlation between number of room and distance between listing and the empire state building 
cor.test(Airbnb_New_York_Data$distance_to._ESB, Airbnb_New_York_Data$price, method = 'pearson')

#6.Number of room v.s. mean price of each listing 
table_for_mean_price <- transform(Airbnb_New_York_Data, mean_price= price / bedrooms)
ggplot(table_for_mean_price, aes(x=bedrooms, y=mean_price)) +
  geom_point(shape=1) + geom_smooth(method=lm, se=FALSE)

#Calculate Correlation between number of room and mean price 
cor.test(table_for_mean_price$bedrooms, table_for_mean_price$mean_price, method = 'pearson')

#6-1 Number of room v.s. price 
ggplot(table_for_mean_price, aes(x=bedrooms, y=price)) +
  geom_point(shape=1) + geom_smooth(method=lm, se=FALSE) + ggtitle("Scatterplot between Airbnb listing price and number of bedroom")

#Calculate Correlation between number of room and mean price >> 0.3100387 
cor.test(table_for_mean_price$bedrooms, table_for_mean_price$price, method = 'pearson')

#7.Accommodates
ggplot(Airbnb_New_York_Data, aes(x=accommodates, y=price)) +
  geom_point(shape=1) + geom_smooth(method=lm, se=FALSE)+ ggtitle("Scatterplot between Airbnb listing price and accommoates")

#Calculate Correlation between accommodates and price >> 0.420095
cor.test(Airbnb_New_York_Data$accommodates, Airbnb_New_York_Data$price, method = 'pearson')
with(Airbnb_New_York_Data, cor.test(accommodates, price, method = 'pearson'))

#8.Overall-satisfaction
ggplot(Airbnb_New_York_Data, aes(x=overall_satisfaction, y=price)) +
  geom_point(shape=1) + geom_smooth(method=lm, se=FALSE)+ ggtitle("Scatterplot between Airbnb listing price and overall satisfaction")

#Calculate Correlation between overall-satisfaction and price >> -0.02979024, p-value=1.438 
cor.test(Airbnb_New_York_Data$overall_satisfaction, Airbnb_New_York_Data$price, method = 'pearson')
with(Airbnb_New_York_Data, cor.test(overall_satisfaction, price, method = 'pearson'))

#Part 3: Looking for the best model to predict the Airbnb listing pricing in NYC
#initial model
linear_model <- lm(formula = price ~ room_type + borough + distance_to._ESB + bedrooms + accommodates + overall_satisfaction, data = Airbnb_New_York_Data)
print(linear_model)
summary(linear_model)

#first stage of selection by r square
linear_model_2 <- lm(formula = price ~ room_type + distance_to._ESB + bedrooms + accommodates + overall_satisfaction, data = Airbnb_New_York_Data)
print(linear_model_2)
summary(linear_model_2)

linear_model_2.1 <- lm(formula = price ~ borough + distance_to._ESB + bedrooms + accommodates + overall_satisfaction, data = Airbnb_New_York_Data)
print(linear_model_2.1)
summary(linear_model_2.1)

linear_model_2.2 <- lm(formula = price ~ room_type + borough + bedrooms + accommodates + overall_satisfaction, data = Airbnb_New_York_Data)
print(linear_model_2.2)
summary(linear_model_2.2)

linear_model_2.3 <- lm(formula = price ~ room_type + borough + distance_to._ESB + accommodates + overall_satisfaction, data = Airbnb_New_York_Data)
print(linear_model_2.3)
summary(linear_model_2.3)

linear_model_2.4 <- lm(formula = price ~ room_type + borough + distance_to._ESB + bedrooms + overall_satisfaction, data = Airbnb_New_York_Data)
print(linear_model_2.4)
summary(linear_model_2.4)

linear_model_2.5 <- lm(formula = price ~ room_type + borough + distance_to._ESB + bedrooms + accommodates, data = Airbnb_New_York_Data)
print(linear_model_2.5)
summary(linear_model_2.5)

#second stage to select model by r square
linear_model_3 <- lm(formula = price ~ borough + distance_to._ESB + bedrooms + accommodates, data = Airbnb_New_York_Data)
print(linear_model_3)
summary(linear_model_3)

linear_model_3.1 <- lm(formula = price ~ room_type + distance_to._ESB + bedrooms + accommodates, data = Airbnb_New_York_Data)
print(linear_model_3.1)
summary(linear_model_3.1)

linear_model_3.2 <- lm(formula = price ~ room_type + borough + bedrooms + accommodates, data = Airbnb_New_York_Data)
print(linear_model_3.2)
summary(linear_model_3.2)

linear_model_3.3 <- lm(formula = price ~ room_type + borough + distance_to._ESB + accommodates, data = Airbnb_New_York_Data)
print(linear_model_3.3)
summary(linear_model_3.3)

linear_model_3.4 <- lm(formula = price ~ room_type + borough + distance_to._ESB + bedrooms, data = Airbnb_New_York_Data)
print(linear_model_3.4)
summary(linear_model_3.4)

#third stage to select model by r square
linear_model_4 <- lm(formula = price ~ distance_to._ESB + bedrooms + accommodates, data = Airbnb_New_York_Data)
print(linear_model_4)
summary(linear_model_4)

linear_model_4.1 <- lm(formula = price ~ room_type + bedrooms + accommodates, data = Airbnb_New_York_Data)
print(linear_model_4.1)
summary(linear_model_4.1)

linear_model_4.2 <- lm(formula = price ~ room_type + distance_to._ESB + accommodates, data = Airbnb_New_York_Data)
print(linear_model_4.2)
summary(linear_model_4.2)

linear_model_4.3 <- lm(formula = price ~ room_type + distance_to._ESB + bedrooms, data = Airbnb_New_York_Data)
print(linear_model_4.3)
summary(linear_model_4.3)

#fourth stage to select model by r square
linear_model_5 <- lm(formula = price ~ distance_to._ESB + accommodates , data = Airbnb_New_York_Data)
print(linear_model_5)
summary(linear_model_5)

linear_model_5.1 <- lm(formula = price ~ room_type + accommodates , data = Airbnb_New_York_Data)
print(linear_model_5.1)
summary(linear_model_5.1)

linear_model_5.2 <- lm(formula = price ~ room_type + distance_to._ESB , data = Airbnb_New_York_Data)
print(linear_model_5.2)
summary(linear_model_5.2)

#Anova test for each variable
anova_result <- aov(price ~ bedrooms, data = Airbnb_New_York_Data)
summary(anova_result)

anova_result2 <- aov(price ~ accommodates, data = Airbnb_New_York_Data)
summary(anova_result2)

anova_result3 <- aov(price ~ distance_to._ESB, data = Airbnb_New_York_Data)
summary(anova_result3)

anova_result4 <- aov(price ~ room_type, data = Airbnb_New_York_Data)
summary(anova_result4)

price.lm = lm(formula = price ~ room_type + distance_to._ESB + bedrooms + accommodates, data = Airbnb_New_York_Data) 
price.res = resid(price.lm)

# create residual plot
plot(Airbnb_New_York_Data$price, price.res, ylab="price", xlab="Residual", main="Residual for price") 



