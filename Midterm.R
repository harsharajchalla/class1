# Load necessary libraries
library(readr) # For reading data
library(dplyr) # For data manipulation
library(ggplot2) # For visualization
library(caret) # For data splitting
library(MLmetrics) # For evaluation metrics
library(glmnet) # For regularization
library(e1071) # For logistic regression

# Load the dataset
data <- read.csv("C:/Users/HP/Downloads/Housing.csv")

# Data exploration and preprocessing
# Assuming the dataset has been preprocessed and cleaned

# Splitting the dataset into train and test sets
set.seed(123) # For reproducibility
train_index <- createDataPartition(data$Sale_Price, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Linear Regression
lm_model <- lm(Sale_Price ~ ., data = train_data)
lm_predictions <- predict(lm_model, newdata = test_data)
lm_rmse <- RMSE(lm_predictions, test_data$Sale_Price)

# Polynomial Regression (Degree 2)
poly_model <- lm(Sale_Price ~ poly(. , 2, raw = TRUE), data = train_data)
poly_predictions <- predict(poly_model, newdata = test_data)
poly_rmse <- RMSE(poly_predictions, test_data$Sale_Price)

# Logistic Regression
# Assuming Sale_Price is categorical (e.g., 0 for low price, 1 for high price)
logistic_model <- glm(Sale_Price ~ ., data = train_data, family = binomial)
logistic_predictions <- predict(logistic_model, newdata = test_data, type = "response")
# Assuming threshold for classification is 0.5
logistic_predictions <- ifelse(logistic_predictions > 0.5, 1, 0)
logistic_accuracy <- Accuracy(logistic_predictions, test_data$Sale_Price)

# Print evaluation metrics
cat("Linear Regression RMSE:", lm_rmse, "\n")
cat("Polynomial Regression RMSE:", poly_rmse, "\n")
cat("Logistic Regression Accuracy:", logistic_accuracy, "\n")
