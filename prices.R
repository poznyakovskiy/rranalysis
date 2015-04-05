data <- read.csv ("data.csv", header = FALSE)
names (data) = c ("name", "location", "amount", "distance", "price", "price.adjusted")
data$name <- as.character (data$name)
data$location <- as.character (data$location)