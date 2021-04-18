#Linear Regression Model with reviews

#here we take reviews and the values show the impact reviews have on the popularity of an Airbnb.

lr_model_2 <- lm(price ~ neighbourhood_group + 
latitude + 
longitude + 
reviews_per_month + 
room_type + minimum_nights  + 
number_of_reviews +
calculated_host_listings_count + 
availability_365, 
data = train_set)

summary_lr_model_2 <- summary(lr_model_2)

summary_lr_model_2

#calculates metrics using summary of model
lr_model_mse_2 <- summary_lr_model_2$sigma^2

lr_model_rsquared_2 <- summary_lr_model_2$r.squared

lr_model_adj_rsquared_2 <- summary_lr_model_2$adj.r.squared

model_results <- bind_rows(model_results, 
            tibble(method = "Linear Regression Model 2", 
      MSE = lr_model_mse_2, 
      R_squared = lr_model_rsquared_2, 
      Adjusted_R_squared = lr_model_adj_rsquared_2))

model_results %>% knitr::kable()
