#Random Forest Model

RFmodel = randomForest(log(price) ~ neighbourhood_group + 
          latitude + 
          longitude + 
      room_type + minimum_nights + 
      number_of_reviews + reviews_per_month + 
      calculated_host_listings_count + 
      availability_365, 
      data = train_set, 
      nodesize = 20, ntree = 200)

#RMSE Prediction
predTest = predict(RFmodel, 
            newdata = test_set)

RMSE_2 = sqrt(mean((predTest - log(test_set$price))^2))

rmse_results <- bind_rows(rmse_results, 
                        tibble(method = "Random Forest Model", 
                                               RMSE = RMSE_2))

rmse_results %>% knitr::kable()
