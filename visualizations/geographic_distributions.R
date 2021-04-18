#Geographic Distribution of Airbnbs in Singapore neighbourhoods

airbnb %>% ggplot(aes(x = latitude, y = longitude, color = neighbourhood)) + 
  geom_point(alpha = 0.3) + theme(legend.position = "none") + labs(x = 'latitude', 
           y = 'longitude', title = 'Geographic Distribution of Airbnbs')


#Geographic Distribution of Airbnbs in Singapore neighbourhoods on basis of room types

airbnb %>% ggplot(aes(x = latitude, y = longitude, color = room_type)) + 
  geom_point(alpha = 0.3) + labs(x = 'latitude', 
      y = 'longitude', title = 'Geographic Distribution of Airbnbs')


#Geographic Distribution of Airbnbs in Singapore neighbourhoods on basis of price

airbnb %>% ggplot(aes(x = latitude, y = longitude, color = price)) + 
  geom_point(alpha = 0.7) + labs(x = 'latitude', y = 'longitude', 
                                 title = 'Geographic Distribution of Airbnbs') + 
  scale_colour_gradient2(low = "yellow", mid = "orange", high = "red", midpoint = 50)


#Geographic Distribution of Airbnbs in Singapore neighbourhoods on basis of availability

airbnb %>% ggplot(aes(x = latitude, y = longitude, color = availability_365)) + 
  geom_point(alpha = 0.7) + labs(x = 'latitude', y = 'longitude', 
                                 title = 'Geographic Distribution of Airbnbs') + 
  scale_colour_gradient2(low = "grey", mid = "aquamarine", 
                         high = "midnightblue", midpoint = 100)
#Higher availability would be due to COVID-19 restrictions in the later months of 2020
