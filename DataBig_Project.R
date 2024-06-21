# Read the data from the context

data <- read.csv('C:/Users/gruja/Downloads/air_pollution_raj.csv', header=TRUE, sep=",")

# Convert the date variable to a date format
data$date <- as.Date(data$date, format="%Y-%m-%d")


# Load necessary libraries
library(ggplot2)
library(forecast)

# Assuming your data frame is named 'data' and contains a 'date' column and a 'Number_of_death_by_respiratory_disease' column

# Convert date variable to proper date format if needed
data$date <- as.Date(data$date)

# Time series plotting
ggplot(data, aes(x = date, y = Number_of_death_by_respiratory_disease)) +
  geom_line() +
  labs(x = "Date", y = "Number of Deaths by Respiratory Disease", title = "Time Series Plot of Respiratory Deaths") +
  theme_minimal()

# Time series decomposition
decomp <- decompose(ts(data$Number_of_death_by_respiratory_disease, frequency = 12)) # Assuming monthly data
plot(decomp)

# Forecasting with ARIMA
model <- auto.arima(ts(data$Number_of_death_by_respiratory_disease, frequency = 12))
forecast <- forecast(model, h = 12) # Forecasting for the next 12 time points
plot(forecast)





# Load necessary libraries
library(rpart)

# Convert date variable to proper date format if needed
data$date <- as.Date(data$date)

# Build a decision tree
tree_model <- rpart(Number_of_death_by_respiratory_disease ~ ., data = data)

# Plot the decision tree
plot(tree_model)
text(tree_model)

# Make predictions using the decision tree
predictions <- predict(tree_model, newdata = data)

# Print the predictions
print(predictions)

