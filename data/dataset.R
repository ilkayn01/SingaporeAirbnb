#Importing dataset
airbnb <- read_csv("http://data.insideairbnb.com/singapore/sg/singapore/2020-02-27/visualisations/listings.csv")

head(airbnb)

summary(airbnb)

#checks for missing features
plot_missing(airbnb)

#2 such columns with over 30% of their data missing

#replaced reviews_per_month with 0 inplace of null values
airbnb$reviews_per_month <- replace_na(airbnb$reviews_per_month, 0)
