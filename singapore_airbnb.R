#Importing libraries
if(!require(readr)) install.packages("readr", 
                                     repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", 
                                         repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", 
                                       repos = "http://cran.us.r-project.org")
if(!require(DataExplorer)) install.packages("DataExplorer", 
                                            repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", 
                                        repos = "http://cran.us.r-project.org")
if(!require(leaps)) install.packages("leaps", 
                                     repos = "http://cran.us.r-project.org")
if(!require(glmnet)) install.packages("glmnet", 
                                      repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", 
                                            repos = "http://cran.us.r-project.org")

library(readr)
library(tidyverse)
library(DataExplorer)
library(ggplot2)
library(corrplot)
library(leaps)
library(glmnet)
library(randomForest)

#Importing dataset
airbnb <- read_csv("http://data.insideairbnb.com/singapore/sg/singapore/2020-02-27/visualisations/listings.csv")
head(airbnb)
summary(airbnb)

#checks for missing features
plot_missing(airbnb)
#2 such columns with over 30% of their data missing

#replaced reviews_per_month with 0 inplace of null values
airbnb$reviews_per_month <- replace_na(airbnb$reviews_per_month, 0)

#Removed unnecessary data and made it ready to use for EDA and model building
airbnb_data_model <- airbnb %>% filter(price < quantile(airbnb$price, 0.9) & 
                price > quantile(airbnb$price, 0.1)) %>% drop_na()
#any value can be taken for set,seed
set.seed(1200)
#new index
airbnb_data_model <- airbnb_data_model %>% mutate(id = row_number())

#Dividing the data into 80% training data and 20% testing data
train_set <- airbnb_data_model %>% sample_frac(.8) %>% filter(price > 0)

test_set  <- anti_join(airbnb_data_model, train_set, by = 'id') %>% filter(price > 0)

nrow(train_set) + nrow(test_set) == nrow(airbnb_data_model %>% filter(price > 0))

#distinct host_id shows the number of unique hosts offering their properties as rentals on Airbnb
n_distinct(airbnb$host_id)

#the highest price of an Airbnb is $10,000
max(airbnb$price)

#Exploratory Data Analysis

#Geographic Distribution of Airbnbs in Singapore neighbourhoods
airbnb %>% ggplot(aes(x = latitude, y = longitude, color = neighbourhood)) + 
  geom_point(alpha = 0.3) + theme(legend.position = "none") + labs(x = 'latitude', 
           y = 'longitude', title = 'Geographic Distribution of Airbnbs')

#Geographic Distribution of Airbnbs in Singapore neighbourhoods on basis of room types
airbnb %>% ggplot(aes(x = latitude, y = longitude, color = room_type)) + 
  geom_point(alpha = 0.3) + labs(x = 'latitude', 
      y = 'longitude', title = 'Geographic Distribution of Airbnbs')

#Range of Airbnbs in every neighbourhood
ggplot(data = airbnb, aes(x = neighbourhood_group)) +
  geom_bar(position = "dodge", aes(fill = room_type)) + 
  scale_fill_manual("Room Type", 
     values = c("#e06f69", "aquamarine", "slateblue", "midnightblue")) +
  labs(x = 'Neighborhood group', y = 'Frequency',
 title = 'Count of types of Airbnbs in every neighborhood') + theme_minimal()

#Geographic Distribution of Airbnbs in Singapore neighbourhoods on basis of price
airbnb %>% ggplot(aes(x = latitude, y = longitude, color = price)) + 
  geom_point(alpha = 0.7) + labs(x = 'latitude', y = 'longitude', 
                                 title = 'Geographic Distribution of Airbnbs') + 
  scale_colour_gradient2(low = "yellow", mid = "orange", high = "red", midpoint = 50)

#Number or reviews neighbourhood wise
ggplot(data = airbnb) + scale_y_log10() + geom_point(aes(x = number_of_reviews,
          y = price, color = neighbourhood_group)) + theme_minimal()

#Availability of Airbnbs yearly
airbnb %>% ggplot(aes(y = availability_365, x = neighbourhood_group)) + 
  geom_boxplot(aes(fill = room_type)) + geom_hline(yintercept = 
       mean(airbnb$availability_365), color = "black", linetype = 2, size = 2) +
  scale_fill_manual("Room Type", values = c("yellow", "red", "#59c6f3", "purple")) +
  labs(x = 'Neighbourhood group', y = 'Availability/year', 
       title = 'Availablity of Airbnbs yearly')
#y-intercept shows the average no. of days a given room is available in the city.

#Geographic Distribution of Airbnbs in Singapore neighbourhoods on basis of availability
airbnb %>% ggplot(aes(x = latitude, y = longitude, color = availability_365)) + 
  geom_point(alpha = 0.7) + labs(x = 'latitude', y = 'longitude', 
                                 title = 'Geographic Distribution of Airbnbs') + 
  scale_colour_gradient2(low = "grey", mid = "aquamarine", 
                         high = "midnightblue", midpoint = 100)
#Higher availability would be due to COVID-19 restrictions in the later months of 2020

#Correlation between 3 key characteristics
airbnb_correlation  <- airbnb  %>% 
  select("availability_365","number_of_reviews","calculated_host_listings_count")
cor(airbnb_correlation)
#experimented with correlation of three primary features to check for a strong correlation.
#no strong correlation found

#Correlation of 8 features
airbnb_cor <- airbnb[, sapply(airbnb, is.numeric)]
#used pearson method out of 3 correlation methods for correlation plot
airbnb_matrix <- cor(airbnb_cor, method = "pearson")
corrplot(airbnb_matrix, type="upper", bg = "#7db5b8", tl.col = "black")

#Median Price Plot
airbnb_median <- airbnb %>% group_by(neighbourhood) %>% summarize(num_listings = n(),
        median_price = median(price), long = median(longitude), lat = median(latitude),
borough = unique(neighbourhood_group))

#compares median prices of Airbnbs across the neighbourhood, with Western Region having the most expensive Airbnb costing 10000 Singapore dollars.
airbnb_median %>% ggplot(aes(x = num_listings, y = median_price, col = borough)) +
  geom_point(alpha = 0.7) + geom_smooth(se = FALSE) + scale_x_log10() + 
  scale_y_log10() + theme_minimal() + labs(title = 'Median Price Plot')

#Model Building

#Linear Regression Model without reviews

#MSE - Mean of squared errors
#R Squared - goodness of fit measure for LR models
#Adjusted R squared helps us determine an optimal model better than just R squared

#First, we make a model without reviews to check how a model with and without reviews would affect our prediction score.
lr_model <- lm(price ~ neighbourhood_group + latitude + longitude + 
room_type + minimum_nights + availability_365, data = train_set)

summary_lr_model <- summary(lr_model)
summary_lr_model

#calculates metrics using summary of model
lr_model_mse <- summary_lr_model$sigma^2

lr_model_rsquared <- summary_lr_model$r.squared

lr_model_adj_rsquared <- summary_lr_model$adj.r.squared

#used tibble instead of data.frame to avoid warning
model_results <- tibble()

model_results <- tibble(method = "Linear Regression Model", MSE = lr_model_mse, 
                        R_squared = lr_model_rsquared, 
                        Adjusted_R_squared = lr_model_adj_rsquared)

model_results %>% knitr::kable()


#Linear Regression Model with reviews

#here we take reviews and the values show the impact reviews have on the popularity of an Airbnb.
lr_model_2 <- lm(price ~ neighbourhood_group + latitude + longitude + 
reviews_per_month + room_type + minimum_nights  + number_of_reviews +
calculated_host_listings_count + availability_365, data = train_set)

summary_lr_model_2 <- summary(lr_model_2)

summary_lr_model_2

#calculates metrics using summary of model
lr_model_mse_2 <- summary_lr_model_2$sigma^2

lr_model_rsquared_2 <- summary_lr_model_2$r.squared

lr_model_adj_rsquared_2 <- summary_lr_model_2$adj.r.squared

model_results <- bind_rows(model_results, tibble(method = "Linear Regression Model 2", 
      MSE = lr_model_mse_2, R_squared = lr_model_rsquared_2, 
      Adjusted_R_squared = lr_model_adj_rsquared_2))

model_results %>% knitr::kable()


#Log Linear Regression Model
log_lr <- lm(formula = log(price) ~ room_type + neighbourhood + reviews_per_month + 
               minimum_nights + number_of_reviews + number_of_reviews +
               log(calculated_host_listings_count), data = train_set)

log_lr_summary <- summary(log_lr)

log_lr_summary

#calculates metrics using summary of model
log_lr_mse <- log_lr_summary$sigma^2

log_lr_rsquared <- log_lr_summary$r.squared

log_lr_adj_rsquared <- log_lr_summary$adj.r.squared

model_results <- bind_rows(model_results, tibble(method = "Log-Linear Regression Model", 
                   MSE = log_lr_mse, R_squared = log_lr_rsquared, 
                   Adjusted_R_squared = log_lr_adj_rsquared))

model_results %>% knitr::kable()

#RMSE Prediction
predTest_log = predict(log_lr, newdata = test_set)
RMSE_1 = sqrt(mean((predTest_log - log(test_set$price))^2))

rmse_results = tibble()

rmse_results <- tibble(method = "Log Linear Regression Model", RMSE = RMSE_1)

#Subset Regression Model with reviews
regression_model <- regsubsets(price ~ neighbourhood_group + latitude + longitude + 
room_type + minimum_nights  + number_of_reviews + reviews_per_month + 
calculated_host_listings_count + availability_365, data = train_set, nbest = 2, nvmax = 9)

summary(regression_model)

#this plot will help us determine which model is the best of all, comparing each by its size. 
plot(regression_model, width = 15)

#based on the above plot, we select a few features that will help us determine an optimal model.
#new feature was added - neighbourhood
subset_model <- lm(price ~ neighbourhood + latitude + longitude + 
      room_type + minimum_nights  + number_of_reviews + reviews_per_month + 
      calculated_host_listings_count + availability_365, data = train_set)

subset_model_summary <- summary(subset_model)

subset_model_summary

#calculates metrics using summary of model
subset_model_mse <- subset_model_summary$sigma^2

subset_model_rsquared <- subset_model_summary$r.squared

subset_model_adj_rsquared <- subset_model_summary$adj.r.squared

model_results <- bind_rows(model_results, tibble(method = "Subset Regression Based Model", 
MSE = subset_model_mse, R_squared = subset_model_rsquared, 
Adjusted_R_squared = subset_model_adj_rsquared))

model_results %>% knitr::kable()

#Higher MSE and lower R squared is undesired and discarded. We need a model which has higher R squared and lower MSE. 
#Such a model will prove as optimal.

#Random Forest Model
RFmodel = randomForest(log(price) ~ neighbourhood_group + latitude + longitude + 
      room_type + minimum_nights  + number_of_reviews + reviews_per_month + 
      calculated_host_listings_count + availability_365, data = train_set, 
      nodesize = 20, ntree = 200)

#RMSE Prediction
predTest = predict(RFmodel, newdata = test_set)
RMSE_2 = sqrt(mean((predTest - log(test_set$price))^2))

rmse_results <- bind_rows(rmse_results, tibble(method = "Random Forest Model", 
                                               RMSE = RMSE_2))

rmse_results %>% knitr::kable()