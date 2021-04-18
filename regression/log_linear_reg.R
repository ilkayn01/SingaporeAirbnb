#Log Linear Regression Model

log_lr <- lm(formula = log(price) ~ room_type + 
neighbourhood + 
reviews_per_month + 
               minimum_nights + 
               number_of_reviews + 
               number_of_reviews +
               log(calculated_host_listings_count), 
               data = train_set)

log_lr_summary <- summary(log_lr)

log_lr_summary

#calculates metrics using summary of model
log_lr_mse <- log_lr_summary$sigma^2

log_lr_rsquared <- log_lr_summary$r.squared

log_lr_adj_rsquared <- log_lr_summary$adj.r.squared

model_results <- bind_rows(model_results, 
  tibble(method = "Log-Linear Regression Model", 
                   MSE = log_lr_mse, 
                   R_squared = log_lr_rsquared, 
                   Adjusted_R_squared = log_lr_adj_rsquared))

model_results %>% knitr::kable()

#RMSE Prediction
predTest_log = predict(log_lr, 
            newdata = test_set)

RMSE_1 = sqrt(mean((predTest_log - log(test_set$price))^2))

rmse_results = tibble()

rmse_results <- tibble(method = "Log Linear Regression Model", 
RMSE = RMSE_1)
