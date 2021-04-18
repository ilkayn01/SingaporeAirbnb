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
