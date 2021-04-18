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
