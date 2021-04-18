#Median Price Plot

airbnb_median <- airbnb %>% group_by(neighbourhood) %>% 
        summarize(num_listings = n(),
        median_price = median(price), 
        long = median(longitude), 
        lat = median(latitude),
        borough = unique(neighbourhood_group))

#compares median prices of Airbnbs across the neighbourhood, with Western Region having the most expensive Airbnb costing 10000 Singapore dollars.

airbnb_median %>% 
ggplot(aes(x = num_listings, 
y = median_price, 
col = borough)) +
  geom_point(alpha = 0.7) + 
  geom_smooth(se = FALSE) + 
  scale_x_log10() + 
  scale_y_log10() + 
  theme_minimal() + 
  labs(title = 'Median Price Plot')
