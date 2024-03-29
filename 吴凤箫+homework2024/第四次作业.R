# Load required libraries
library(caret)
library(randomForest)

# Load the dataset
data(mtcars)

# Data preparation and pre-process
# Convert cyl and vs to factors
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$vs <- as.factor(mtcars$vs)

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(mtcars$mpg, p = 0.8, list = FALSE)
trainData <- mtcars[trainIndex, ]
testData <- mtcars[-trainIndex, ]

# Feature selection and visualization (if needed)
# For simplicity, we will use all variables as features

# Training the random forest model
model <- train(
  mpg ~ .,
  data = trainData,
  method = "rf",
  trControl = trainControl(method = "cv"),
  tuneLength = 3
)

# Print the best model parameters
print(model)

# Evaluate the model
predictions <- predict(model, testData)
rmse <- RMSE(predictions, testData$mpg)
r_squared <- R2(predictions, testData$mpg)

cat("RMSE: ", rmse, "\n")
cat("R-squared: ", r_squared, "\n")
