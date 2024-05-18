library(ggplot2)

# Read the csv file
file_path <- "C:/Users/HP/Downloads/Titanic-Dataset.csv"
data <- read.csv(file_path)
str(data)

# Linear Regression: Using 'Age' to predict 'Fare'
model_linear <- lm(Fare ~ Age, data=data)

# Predictions
data$pred_linear <- predict(model_linear, newdata=data)

# Plotting Linear Regression
plot(data$Age, data$Fare, main="Linear Regression: Age vs Fare",
     xlab="Age", ylab="Fare", col="black", pch=13)
lines(data$Age, data$pred_linear, col="yellow", lwd=2)
legend("topright", legend=c("Actual data", "Fitted line"), col=c("black", "yellow"), pch=c(16, NA), lty=c(NA, 1))



# Linear Regression: Using 'Survived' to predict 'Fare'
model_linear <- lm(Fare ~ Survived, data=data)

# Predictions
data$pred_linear <- predict(model_linear, newdata=data)

# Plotting Linear Regression
plot(data$Survived, data$Fare, main="Linear Regression: Survived vs Fare",
     xlab="Survived", ylab="Fare", col="black", pch=13)
lines(data$Survived, data$pred_linear, col="yellow", lwd=2)
legend("topright", legend=c("Actual data", "Fitted line"), col=c("black", "yellow"), pch=c(16, NA), lty=c(NA, 1))



# Polynomial Regression: Using 'Fare' to predict 'Age' with a degree 3 polynomial
model_poly <- lm(Age ~ poly(Fare, 3), data=data)

# Predictions for Polynomial Regression
data <- data[order(data$Fare), ]  # Order the data by Fare
data$pred_poly <- predict(model_poly, newdata=data)

# Plotting Polynomial Regression
plot(data$Fare, data$Age, main="Polynomial Regression: Fare vs Age",
     xlab="Fare", ylab="Age", col="red", pch=16)
lines(data$Fare, data$pred_poly, col="green", lwd=3)
legend("topright", legend=c("Actual data", "Fitted polynomial"), col=c("red", "green"), pch=c(16, NA), lty=c(NA, 1))


# Polynomial Regression: Using 'Fare' to predict 'Survived' with a degree 3 polynomial
model_poly <- lm(Survived ~ poly(Fare, 3), data=data)

# Predictions for Polynomial Regression
data <- data[order(data$Fare), ]  # Order the data by Fare
data$pred_poly <- predict(model_poly, newdata=data)

# Plotting Polynomial Regression
plot(data$Fare, data$Survived, main="Polynomial Regression: Fare vs Survived",
     xlab="Fare", ylab="Survived", col="red", pch=16)
lines(data$Fare, data$pred_poly, col="green", lwd=3)
legend("topright", legend=c("Actual data", "Fitted polynomial"), col=c("red", "green"), pch=c(16, NA), lty=c(NA, 1))




# Create a binary variable 'Age_high' (1 if Age > median, else 0)
data$Age_high <- ifelse(data$Age > median(data$Age), 1, 0)

# Check the structure and preview the data
str(data)
head(data)
# Data preprocessing
# Create a binary variable 'Survived' (1 if survived, else 0)
data <- data %>%
  select(Survived, Pclass, Sex, Age, Fare) %>%
  mutate(Sex = ifelse(Sex == "male", 1, 0)) %>%
  filter(!is.na(Age), !is.na(Fare))

# Logistic Regression: Using 'Fare', 'Pclass', and 'Sex' to predict 'Survived'
model_logistic <- glm(Survived ~ Fare + Pclass + Sex + Age, data = data, family = binomial)

# Summary of the model
summary(model_logistic)

# Predictions for Logistic Regression
data$pred_logistic <- predict(model_logistic, type = "response")
data$pred_class <- ifelse(data$pred_logistic > 0.5, 1, 0)

# Confusion matrix to evaluate the model
confusion_matrix <- table(Predicted = data$pred_class, Actual = data$Survived)
print(confusion_matrix)

# Accuracy
accuracy <- mean(data$pred_class == data$Survived)
print(paste("Accuracy:", accuracy))

# Plotting Logistic Regression using ggplot2
ggplot(data, aes(x = Fare, y = Survived)) +
  geom_jitter(width = 0.1, height = 0.1, color = "black", alpha = 0.5) +
  geom_jitter(aes(y = pred_class), width = 0.1, height = 0.1, color = "red", alpha = 0.5) +
  labs(title = "Logistic Regression: Fare vs Survived",
       x = "Fare",
       y = "Survived") +
  theme_minimal() +
  scale_y_continuous(breaks = c(0, 1), labels = c("Not Survived", "Survived"))

