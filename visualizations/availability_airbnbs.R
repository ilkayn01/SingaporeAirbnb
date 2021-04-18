#Range of Airbnbs in every neighbourhood

ggplot(data = airbnb, 
aes(x = neighbourhood_group)) +
  geom_bar(position = "dodge", 
  aes(fill = room_type)) + 
  scale_fill_manual("Room Type", 
     values = c("#e06f69", "aquamarine", "slateblue", 
     "midnightblue")) +
  labs(x = 'Neighborhood group', 
  y = 'Frequency',
 title = 'Count of types of Airbnbs in every neighborhood') + 
 theme_minimal()
 

#Number or reviews neighbourhood wise

ggplot(data = airbnb) + 
scale_y_log10() + 
geom_point(aes(x = number_of_reviews,
          y = price, 
          color = neighbourhood_group)) + 
          theme_minimal()


#Availability of Airbnbs yearly

airbnb %>% ggplot(aes(y = availability_365, 
x = neighbourhood_group)) + 
  geom_boxplot(aes(fill = room_type)) + 
  geom_hline(yintercept = 
       mean(airbnb$availability_365), 
       color = "black", linetype = 2, 
       size = 2) +
  scale_fill_manual("Room Type", 
  values = c("yellow", "red", "#59c6f3", "purple")) +
  labs(x = 'Neighbourhood group', 
  y = 'Availability/year', 
       title = 'Availablity of Airbnbs yearly')
       
#y-intercept shows the average no. of days a given room is available in the city.
