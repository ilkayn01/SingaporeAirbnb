#Subset Regression Model with reviews

regression_model <- regsubsets(price ~ neighbourhood_group + 
                latitude + 
                longitude + 
                  room_type + 
                  minimum_nights + 
                  number_of_reviews + 
                  reviews_per_month + 
calculated_host_listings_count + 
          availability_365, 
          data = train_set, 
          nbest = 2, nvmax = 9)

summary(regression_model)

#this plot will help us determine which model is the best of all, comparing each by its size. 
plot(regression_model, width = 15)

#based on the above plot, we select a few features that will help us determine an optimal model.

#new feature was added - neighbourhood

subset_model <- lm(price ~ neighbourhood + 
        latitude + longitude + 
      room_type + minimum_nights  + 
      number_of_reviews + 
      reviews_per_month + 
      calculated_host_listings_count + 
      availability_365, 
      data = train_set)

subset_model_summary <- summary(subset_model)

subset_model_summary

#calculates metrics using summary of model
subset_model_mse <- subset_model_summary$sigma^2

subset_model_rsquared <- subset_model_summary$r.squared

subset_model_adj_rsquared <- subset_model_summary$adj.r.squared

model_results <- bind_rows(model_results, tibble(method = "Subset Regression Based Model", 
MSE = subset_model_mse, 
        R_squared = subset_model_rsquared, 
                  Adjusted_R_squared = subset_model_adj_rsquared))

model_results %>% knitr::kable()

#Higher MSE and lower R squared is undesired and discarded. We need a model which has higher R squared and lower MSE. 
#Such a model will prove as optimal.
