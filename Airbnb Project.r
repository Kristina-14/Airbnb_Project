# loading the required libraries
if(!require(pacman)) install.packages("pacman", repos = "http://cran.us.r-project.org")
library(pacman)
pacman::p_load(tidyverse, readr, data.table, caret, lubridate, 
               ggthemes, ggplot2, glmnet, scales, stringr, dplyr, ggmap, ggcorrplot, 
               treemapify, rpart, nnet, formatR, rmarkdown, knitr)
library(ggmap)

# loading the required data
airbnb <- read.csv("C:/Users/Hp1/Downloads/seattle_01.csv")

options(digits = 4)

#No. of row and cols the data has - shape of the data
dim(airbnb)

#Names of the cols in the dataset
names(airbnb)

#display the summary
summary(airbnb)
str(airbnb)

#display the data
head(airbnb)

#airbnb <-as.data.frame(airbnb)
class(airbnb)

#check if there is any null values
sum(is.na(airbnb))
colSums(is.na(airbnb))

#Create a dataframe na_bar to plot the NAs:
na_vis <- data.frame(t(colSums(is.na(airbnb))))
na_bar <- data.frame(Features = names(na_vis),totals=colSums(na_vis))

#visual NA distribution 
na_bar %>% ggplot(aes(x = reorder(Features, totals), y = totals, fill = Features, label = totals))+
  geom_bar(stat = "identity")+
  ggtitle("NA Distribution")+
  xlab("Features")+
  ylab("Total NAs")+
  coord_flip()+
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

## Data Cleaning
#Since the NA values are of no use so we need to remove NA
airbnb$overall_satisfaction[is.na(airbnb$overall_satisfaction)] <- mean(airbnb$overall_satisfaction, na.rm = TRUE)

mean(airbnb$overall_satisfaction)
#The mean comes out to be 4.841

head(airbnb$overall_satisfaction)
#so there are no NAs

#checking for the null values
sum(is.na(airbnb))

# We can see that there are 2 NA values and we need to remove them as well
airbnb <-airbnb %>% replace_na(list(bathrooms = 0))

#checking again
sum(is.na(airbnb))

#Feature Selection
names(airbnb)

head(airbnb$X)
head(airbnb$room_id)
head(airbnb$host_id)
airbnb %>% select(address) %>% distinct()

#Removing redundancy 
address_clean <-gsub("Seattle, WA, United States", "Seattle",
                gsub("Kirkland, WA, United States", "Kirkland",
                gsub("Bellevue, WA, United States", "Bellevue",
                gsub("Redmond, WA, United States", "Redmond",
                gsub("Mercer Island, WA, United States", "Mercer Island",
                gsub("Seattle, WA", "Seattle",
                gsub("Renton, WA, United States", "Renton",
                gsub("Ballard, Seattle, WA, United States", "Seattle",
                gsub("West Seattle, WA, United States", "Seattle",
                gsub("Medina, WA, United States", "Medina",
                gsub("Newcastle, WA, United States", "Newcastle",
                gsub("Seattle , WA, United States", "Seattle",
                gsub("Ballard Seattle, WA, United States", "Seattle",
                gsub("Yarrow Point, WA, United States", "Yarrow Point",
                gsub("Clyde Hill, WA, United States", "Clyde Hill",
                gsub("Tukwila, WA, United States", "Tukwila",
                gsub("Seattle, Washington, US, WA, United States", "Seattle",
                gsub("Capitol Hill, Seattle, WA, United States", "Seattle",
                gsub("Kirkland , Wa, United States", "Kirkland",
                gsub("Hunts Point, WA, United States", "Hunts Point",
                gsub("Seattle, DC, United States", "Seattle",
                gsub("Seattle, United States", "Seattle", gsub("Vashon, WA, United States", "Vashon",
                gsub("Kirkland , WA, United States", "Kirkland",
                gsub("Bothell, WA, United States", "Bothell",
                gsub("Washington, WA, United States", "Seattle",
                airbnb$address))))))))))))))))))))))))))

address_clean2 <-gsub(".*WA*.", "Seattle", address_clean)

# Reassigning the column to the feature "address":
airbnb$address <-gsub("Seattle, United States", "Seattle", 
                      gsub("Seattle United States", "Seattle", address_clean2))

city_list <-airbnb %>% group_by(address) %>% summarize(listing_sum = n()) %>%
  arrange(-listing_sum)
city_list
# There are 14 different cities

city_list %>% 
  ggplot(aes(x = reorder(address, listing_sum), y = listing_sum, 
             fill = address, label = listing_sum))+
  geom_bar(stat = "identity")+
  ggtitle("Location Distribution")+
  xlab("Location")+
  ylab("Total Listings")+
  coord_flip()+
  geom_text(size = 4, 
            position = position_stack(vjust = 0.5))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

head(airbnb$last_modified)
head(airbnb$location)
head(airbnb$name)
airbnb %>% select(currency) %>% distinct()
airbnb %>% select(rate_type) %>% distinct()

# Create the cleaned dataset
airbnb <-airbnb %>% select(-c(X, room_id, host_id, last_modified,
                              location, name, currency, rate_type)) %>%
  rename(city = address, rating = overall_satisfaction,
         reviews_sum = reviews)
# reorganising the cols
airbnb <-airbnb[,c(8, 2, 4, 3, 1, 6, 7, 5, 9, 10)]
#all the 10 required fields 
names(airbnb)

head(airbnb)

# Explanatory Data Analysis
# Correlogram

airbnb_num <-airbnb %>% select(-c(city, room_type))
#correlation matrix
airbnb_cor <-cor(airbnb_num)

ggcorrplot(airbnb_cor)+
  labs(title = "Airbnb Correlogram")+
  theme(plot.title = element_text(hjust = 0.5))


#Densitation Plot of Price Distribution below $300:
airbnb %>% filter(price <=300) %>% ggplot(aes(price))+
  geom_density(fill = "deepskyblue", size = 1.5, color = "navyblue", alpha = 0.5)+
  xlab("Price")+
  ylab("Density")+
  ggtitle("Price Distribution at or Below $300")+
  theme(plot.title = element_text(hjust = 0.5))

#Geographical Scatterplot of Prices in Seattle
seattle_map <- get_stamenmap(bbox = c(left = -122.5, bottom = 47.49, 
                                      right = -122.09, top = 47.74), 
                             zoom = 9, maptype = "toner")

summary(airbnb$price)

quantile(airbnb$price)
sum(airbnb$price <=300)/length(airbnb$price)

#Filter the dataframe airbnb_map
airbnb_map <-airbnb %>% filter(price <=300)

#Visualize a "Heatmap" of Seattle with all listing prices included:
ggmap(seattle_map, extent= "normal")+
  geom_point(data = airbnb, 
             aes(x = longitude, y = latitude, color = price), 
             size = 1.5, alpha = .6)+
  scale_color_gradientn(colors = c("mediumblue", "lawngreen", "blueviolet", "red"),
                        values = scales::rescale(c(.003, .013, .0176, .025, .2, .3, .4)))+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("Seattle Location Pricing")+
  theme(plot.title = element_text(hjust = 0.5),  
        panel.border = element_rect(color = "gray", fill=NA, size=3))

# Plot the Filtered Heat Map of Seattle: (less than or equal to $300)
ggmap(seattle_map, extent= "normal")+
  geom_point(data = airbnb_map, 
             aes(x = longitude, y = latitude, color = price), 
             size = 1.5, alpha = .6)+
  scale_color_gradientn(colours = rainbow(5))+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("Seattle Pricing at or Below $300")+
  theme(plot.title = element_text(hjust = 0.5),  
        panel.border = element_rect(color = "gray", fill=NA, size=3))

sum(airbnb$price >= 50 & airbnb$price <= 150)/length(airbnb$price)

# Treemap

city_distribution <-airbnb %>% group_by(city) %>% summarize(listing_sum = n()) %>%
  arrange(-listing_sum)
# Add column "tmlab" for Treemap labels
city_distribution <-city_distribution %>% 
  unite("tmlab", city:listing_sum, sep = " ", remove = FALSE)
# Plot a Treemap to visualize the distribution of listings by city:
city_distribution %>% ggplot(aes(area = listing_sum, fill = city, label = tmlab))+
  geom_treemap()+
  geom_treemap_text(fontface = "italic", col = "white", place = "center",
                    grow = TRUE)

# Visualize Price by City

city_price <-airbnb %>% group_by(city) %>% 
  summarize(mean_price = mean(price),
            listing_sum = n()) %>%
  arrange(-mean_price) %>% mutate(mean_price = sprintf("%0.1f", mean_price))
# Coerce the "mean_price" to integer:
city_price$mean_price <-as.integer(city_price$mean_price)

# Plot the Visualization:

city_price %>% ggplot(aes(x = reorder(city, mean_price), y = mean_price, 
                          fill = city, label = mean_price))+
  geom_bar(stat = "identity")+
  coord_flip()+
  xlab("City")+
  ylab("Mean Price")+
  ggtitle("Mean Price per Night by City")+
  geom_text(size = 4, 
            position = position_stack(vjust = 0.5))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

# Visualize Mean Price & Sum of Listings by City:
# Create dataframe "city_comp" with a percentage column:
city_comp <-city_price %>% 
  mutate(percentage = sprintf("%0.3f",(listing_sum/sum(listing_sum)*100)))
# Add the % symbol to the percentage feature:

city_comp$percentage <- paste(city_comp$percentage, "%")

# Combine the mean price & percentage values into one column:
city_comp <-city_comp %>% 
  unite("citylab", mean_price, percentage, sep = ", ", remove = FALSE)

# Plot the visualization with City and Mean Price. Percentage of Total Listings as labels:

city_comp %>% 
  ggplot(aes(x = reorder(city, mean_price), y = mean_price, 
             fill = city, label = citylab))+
  geom_bar(stat = "identity")+
  coord_flip()+
  xlab("City")+
  ylab("Mean Price")+
  ggtitle("Mean Price & Percentage of Total Listings by City")+
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

head(airbnb$rating)

rating_comp <-airbnb %>% group_by(city) %>% 
  summarize(mean_rating = mean(rating), mean_price = mean(price)) %>%
  select(city, mean_rating, mean_price)

# Set the parameters for the dual-axis plot:
ylim_1 <-c(0,10)
ylim_2 <-c(70, 400)
b <- diff(ylim_1)/diff(ylim_2)
a <- b*(ylim_1[1] - ylim_2[1])

# Plot the Barplot (Rating) with Overlapping Line (Price):
ggplot(rating_comp, aes(city, group =1))+
  geom_bar(aes(y=mean_rating), stat="identity", color = "navyblue", alpha=.7)+
  geom_line(aes(y = a + mean_price*b), color = "red", size = 2)+
  scale_y_continuous(name = "Mean Rating", 
                     sec.axis = sec_axis(~ (. - a)/b, name = "Mean Price"))+
  xlab("City")+
  ggtitle("Rating & Price Comparison by City")+
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        plot.title = element_text(hjust = 0.5))

range(rating_comp$mean_rating)

airbnb %>% filter(price <= 1000) %>% 
  ggplot(aes(x = reviews_sum, y = price))+ 
  geom_point(color="navyblue", alpha = 0.6, size = 1.5)+
  xlab("Number of Reviews")+
  ylab("Price")+
  ggtitle("Review vs Price Distribution")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))


airbnb %>% group_by(room_type) %>%
  summarize(mean_price = mean(price)) %>%
  ggplot(aes(reorder(room_type, mean_price), 
             y = mean_price, label=sprintf("%0.2f", 
                                           round(mean_price, digits = 2))))+ 
  geom_bar(stat = "identity", color = "navyblue", 
           size = 1.5, fill = "deepskyblue")+
  coord_flip()+
  xlab("Room Type")+
  ylab("Mean Price")+
  ggtitle("Mean Price by Room Type")+
  geom_text(size = 3,
            position = position_stack(vjust = 0.5))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
######################################
airbnb %>% group_by(bathrooms) %>% summarize(mean_price = mean(price)) %>%
  ggplot(aes(bathrooms, mean_price))+
  geom_bar(stat = "identity", fill = "deepskyblue", color = "navyblue", size = 1.2)+
  xlab("Number of Bathrooms")+
  ylab("Mean Price per Night")+
  ggtitle("Mean Price per Number of Bathrooms")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

airbnb %>% group_by(bathrooms) %>% summarize(sum_bath = length(bathrooms)) %>%
  ggplot(aes(reorder(bathrooms, sum_bath), y=sum_bath, label = sum_bath))+
  geom_bar(stat = "identity", fill = "deepskyblue", color = "cyan", size = 1.2)+
  coord_flip()+
  geom_text(size = 5, color = "navyblue",
            position = position_stack(vjust = 0.5))+
  xlab("Number of Bathrooms")+
  ylab("Total Number of Listings")+
  ggtitle("Listing Distribution by Bathroom Total")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

airbnb %>% group_by(bedrooms) %>% 
  summarize(mean_price = mean(price)) %>%
  ggplot(aes(bedrooms, mean_price))+
  geom_bar(stat = "identity", 
           color = "navyblue", fill = "deepskyblue", size = 1.5)+
  xlab("Number of Bedrooms")+
  ylab("Mean Price per Night")+
  ggtitle("Mean Price Distribution by Bedroom Total")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

airbnb %>% group_by(bedrooms) %>% summarize(sum_beds = length(bedrooms)) %>%
  ggplot(aes(reorder(bedrooms, sum_beds), y = sum_beds, label = sum_beds))+
  geom_bar(stat = "identity", 
           color = "cyan", fill = "deepskyblue", size = 1.5)+
  coord_flip()+
  geom_text(size = 5, color = "navyblue",
            position = position_stack(vjust = 0.5))+
  xlab("Total Number of Listings")+
  ylab("Listing Distribution by Bedroom Total")+
  ggtitle("Number of Bedrooms")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

airbnb %>% group_by(accommodates) %>% summarize(mean_price = mean(price)) %>%
  ggplot(aes(accommodates, mean_price))+ 
  geom_bar(stat = "identity", color = "navyblue", 
           size = 2, fill = "deepskyblue")+
  xlab("Accommodates")+
  ylab("Mean Price")+
  ggtitle("Number of Guests Accommodated & Mean Price")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

airbnb %>% group_by(accommodates) %>% 
  summarize(sum_acc = length(accommodates)) %>% 
  ggplot(aes(x = factor(accommodates), y = sum_acc))+
  geom_bar(stat = "identity", color = "navyblue", fill = "deepskyblue", size = 1.5)+
  xlab("Number of Guests Accommodated")+
  ylab("Total Number of Listings")+
  ggtitle("Listings Sum by Accommodation Total")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

# Modeling

# Set the seed for reproducibility:
set.seed(123, sample.kind = "Rounding")
test_index <- createDataPartition(y = airbnb$price, times = 1, p = 0.1, list = F)
airbnb_combined <- airbnb[-test_index,]
airbnb_test <- airbnb[test_index,]
# Remove test_index:
rm(test_index)

set.seed(123, sample.kind = "Rounding")
test_index <- createDataPartition(y = airbnb_combined$price, times = 1, 
                                  p = 0.2, list = F)
airbnb_train <- airbnb_combined[-test_index,]
validation <- airbnb_combined[test_index,]

# Remove test_index once again:
rm(test_index)

# The Loss Function / RMSE
RMSE <-function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

RMSE <-function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

airbnb_train_median <-median(airbnb_train$price)

MM_RMSE <-RMSE(validation$price, airbnb_train_median)
results_table <-tibble(Model_Type = "Baseline Median", 
                       RMSE = MM_RMSE) %>% 
  mutate(RMSE = sprintf("%0.2f", RMSE))
knitr::kable(results_table)

airbnb_form <-price ~ rating + reviews_sum + bedrooms + bathrooms + 
  accommodates + latitude + longitude

# Linear Model
lm_airbnb <- lm(airbnb_form, data = airbnb_train)

# Create the prediction
lm_preds <-predict(lm_airbnb, validation)

# Table the Results
LM_RMSE <-RMSE(validation$price, lm_preds)
results_table <-tibble(Model_Type = c("Baseline Median", "Linear"), 
                       RMSE = c(MM_RMSE, LM_RMSE)) %>% 
  mutate(RMSE = sprintf("%0.2f", RMSE))

knitr::kable(results_table)

# Elastic Net Regression with glmnet

# Set the seed for reproducibility:
set.seed(123, sample.kind = "Rounding")
train_enr <- train(airbnb_form, data = airbnb_train, method = "glmnet",
                   preProcess = c("center", "scale"),
                   tuneLength = 10, trace = F)

# Confirm the optimal alpha and lambda parameters
train_enr$bestTune
# alpha = 0.1, and lambda = 21.24

# Create the Prediction:
elastic_preds <-predict(train_enr, validation)

# Table the Results
ENR_RMSE <-RMSE(validation$price, elastic_preds)
results_table <-tibble(Model_Type = c("Baseline Median", "Linear",
                                      "Elastic Net Regression"), 
                       RMSE = c(MM_RMSE,LM_RMSE, ENR_RMSE)) %>% 
  mutate(RMSE = sprintf("%0.2f", RMSE))
knitr::kable(results_table)

#Regression Tree Model with rpart
# Set the seed for reproducibility:
set.seed(123, sample.kind = "Rounding")
train_rpart <- train(airbnb_form, method = "rpart", data = airbnb_train,
                     tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)),
                     preProcess = c("center", "scale"))

# Check the bestTune to find the final complexity parameter used for the model:
train_rpart$bestTune
# The final cp is 0.00625.

# Create the Prediction:
rt_preds <-predict(train_rpart, validation)

# Table the Results:
RT_RMSE <-RMSE(validation$price, rt_preds)
results_table <-tibble(Model_Type = c("Baseline Median", "Linear",
                                      "Elastic Net Regression", 
                                      "Regression Tree"), 
                       RMSE = c(MM_RMSE,LM_RMSE, ENR_RMSE, RT_RMSE)) %>% 
  mutate(RMSE = sprintf("%0.2f", RMSE))
knitr::kable(results_table)

# Random Forest Model
# Set the tuneGrid parameters: 
rf_tune <- expand.grid(.mtry=c(1:3))

# Set the seed for reproducibility:
set.seed(123, sample.kind = "Rounding")
train_rf <- train(airbnb_form, data = airbnb_train,
                  method = "rf", ntree = 150,
                  tuneGrid = rf_tune, nSamp = 1000, 
                  preProcess = c("center","scale"))

# Check the bestTune:
train_rf$bestTune
# The bestTune is a mtry of 1

# Create the Prediction:
rf_preds <-predict(train_rf, validation)

# Table the Results
RF_RMSE <-RMSE(validation$price, rf_preds)
results_table <-tibble(Model_Type = c("Baseline Median", "Linear",
                                      "Elastic Net Regression", 
                                      "Regression Tree", "Random Forest"), 
                       RMSE = c(MM_RMSE,LM_RMSE, ENR_RMSE, RT_RMSE,
                                RF_RMSE)) %>% 
  mutate(RMSE = sprintf("%0.2f", RMSE))
knitr::kable(results_table)

# "Bagging Tree" -- Bootstrap Aggregating Model
# Set the seed for reproducibility:
set.seed(123, sample.kind = "Rounding")
train_bag <-train(airbnb_form, data = airbnb_train, method = "treebag",
                  importance = T, tuneLength = 10, 
                  preProcess = c("center","scale"))

# Create the Prediction
bag_preds <-predict(train_bag, validation)

# Table the Results
BAG_RMSE <-RMSE(validation$price, bag_preds)
results_table <-tibble(Model_Type = c("Baseline Median", "Linear",
                                      "Elastic Net Regression", 
                                      "Regression Tree", "Random Forest",
                                      "BAG"), 
                       RMSE = c(MM_RMSE,LM_RMSE, ENR_RMSE, RT_RMSE,
                                RF_RMSE, BAG_RMSE)) %>% 
  mutate(RMSE = sprintf("%0.2f", RMSE))
knitr::kable(results_table)

# kNN Regression Model
# Set the seed for reproducibility:
set.seed(123, sample.kind = "Rounding")
train_knn <- train(airbnb_form, method = "knn", data = airbnb_train,
                   tuneLength = 5, preProcess = c("center","scale"))

# Find best value for k:
train_knn$bestTune
# k = 13 is the final value used for the model.

# Create the Prediction
knn_preds <- predict(train_knn, validation)

# Table the Results
kNN_RMSE <-RMSE(validation$price, knn_preds)
results_table <-tibble(Model_Type = c("Baseline Median", "Linear",
                                      "Elastic Net Regression", 
                                      "Regression Tree", "Random Forest",
                                      "BAG", "kNN"), 
                       RMSE = c(MM_RMSE,LM_RMSE, ENR_RMSE, RT_RMSE,
                                RF_RMSE, BAG_RMSE, kNN_RMSE)) %>% 
  mutate(RMSE = sprintf("%0.2f", RMSE))
knitr::kable(results_table)

#Neural Net Model
# Create the tuneGrid parameters:
NN_grid <-expand.grid(size=c(1, 5, 20), decay = c(0, 0.01, 0.1))

# Set the seed for reproducibility:
set.seed(123, sample.kind = "Rounding")
train_NN <-train(airbnb_form, data = airbnb_train, method= "nnet", 
                 linout = T, trace = F, tuneGrid = NN_grid,
                 preProc = c("center", "scale"))

# Check bestTune:
train_NN$bestTune
# The optimal size is 5 and decay = 0.01

# Create the Prediction:
NN_preds <-predict(train_NN, validation)

# Table the Results:
NN_RMSE <-RMSE(validation$price, NN_preds)
results_table <-tibble(Model_Type = c("Baseline Median", "Linear",
                                      "Elastic Net Regression", 
                                      "Regression Tree", "Random Forest",
                                      "BAG", "kNN", "Neural Net"), 
                       RMSE = c(MM_RMSE,LM_RMSE, ENR_RMSE, RT_RMSE,
                                RF_RMSE, BAG_RMSE, kNN_RMSE, NN_RMSE)) %>% 
  mutate(RMSE = sprintf("%0.2f", RMSE))
knitr::kable(results_table)

#Final Model
# Random Forest Model (with test set)

# Set the mtry to 1 as determined from the previous tuning on airbnb_train:
tune_grid_rf <-expand.grid(mtry = 1)

# Set the seed for reproducibility:
set.seed(123, sample.kind = "Rounding")
train_rf_final <- train(airbnb_form, data = airbnb_combined,
                        method = "rf", ntree = 150,
                        tuneGrid = tune_grid_rf, nSamp = 1000, 
                        preProcess = c("center","scale"))

# Create the Prediction:
rf_preds_final <-predict(train_rf_final, airbnb_test)

# Table the Results
RFF_RMSE <-RMSE(airbnb_test$price, rf_preds_final)
results_table <-tibble(Model_Type = c("Baseline Median", "Linear",
                                      "Elastic Net Regression", 
                                      "Regression Tree", "Random Forest",
                                      "BAG", "kNN", "Neural Net",
                                      "Random Forest Final (Test Set)"), 
                       RMSE = c(MM_RMSE,LM_RMSE, ENR_RMSE, RT_RMSE,
                                RF_RMSE, BAG_RMSE, kNN_RMSE, NN_RMSE,
                                RFF_RMSE)) %>% 
  mutate(RMSE = sprintf("%0.2f", RMSE))

#Results
knitr::kable(results_table)
