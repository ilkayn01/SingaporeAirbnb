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
