#libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(readxl)

Final_DataSet <- read_excel("C:/Users/kpasalkar7588/Downloads/Final DataSet1.xlsx")

summary(Final_DataSet)

# Check for missing values
missing_values <- colSums(is.na(Final_DataSet))
print(missing_values)
dim(Final_DataSet)

#Count of total missing values
print("Count of total missing values is :-")
sum(is.na(Final_DataSet))

#Omit Missing values
dataset = na.omit(Final_DataSet)
summary(dataset)

#Checking missing values again
missing_values <- colSums(is.na(dataset))
print(missing_values)
dim(dataset)

univariate_vars <- c("Gender", "EthnicGroup", "ParentEduc", "LunchType", 
                     "TestPrep", "ParentMaritalStatus", "PracticeSport", 
                     "IsFirstChild", "NrSiblings", "TransportMeans", "WklyStudyHours")

for (var in univariate_vars) {
  if (is.numeric(dataset[[var]])) {
    cat("\nSummary statistics for", var, ":\n")
    print(summary(dataset[[var]]))
    
    # Histogram
    hist_plot <- ggplot(dataset, aes_string(x = var)) +
      geom_histogram(fill = "skyblue", color = "black") +
      labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
      theme_minimal()
    
    # Boxplot
    box_plot <- ggplot(dataset, aes_string(y = var)) +
      geom_boxplot(fill = "lightgreen", color = "black") +
      labs(title = paste("Boxplot of", var), y = var) +
      theme_minimal()
    
    plots[[paste(var, "_hist")]] <- hist_plot
    plots[[paste(var, "_box")]] <- box_plot
    
  } 
  else {
    # For categorical variables (e.g., Gender, EthnicGroup, etc.)
    cat("\nFrequency table for", var, ":\n")
    print(table(dataset[[var]]))
    
    # Bar plot (for categorical variables)
    bar_plot <- ggplot(dataset, aes_string(x = var)) +
      geom_bar(fill = "orange", color = "black") +
      labs(title = paste("Bar plot of", var), x = var, y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed
    
    plots[[paste(var, "_bar")]] <- bar_plot
  }
  
  # Checking missing values
  cat("\nMissing values in", var, ":", sum(is.na(dataset[[var]])), "\n")
}

for (var in univariate_vars) {
  if (is.numeric(dataset[[var]])) {
    print(plots[[paste(var, "_hist")]])
    print(plots[[paste(var, "_box")]])
  } else {
    print(plots[[paste(var, "_bar")]])
  }
}

# Bivariate Analysis

# List of numeric variables of interest
numeric_vars <- c("MathScore", "ReadingScore", "WritingScore")

# Scatter plots for pairs of numeric variables
for (i in 1:length(numeric_vars)) {
  for (j in (i+1):length(numeric_vars)) {
    var1 <- numeric_vars[i]
    var2 <- numeric_vars[j]
    plot_title <- paste("Scatter Plot of", var1, "vs.", var2)
    
    print(ggplot(dataset, aes_string(x = var1, y = var2)) +
            geom_point(color = "blue") +
            labs(title = plot_title, x = var1, y = var2) +
            theme_minimal())
  }
}

# Box plots for each numeric variable
for (var in numeric_vars) {
  plot_title <- paste("Box Plot of", var)
  
  print(ggplot(dataset, aes_string(y = var)) +
          geom_boxplot(fill = "lightgreen", color = "black") +
          labs(title = plot_title, y = var) +
          theme_minimal())
}


# Example t-test between two groups (e.g., ReadingScore for males vs. females)
t_test_result <- t.test(ReadingScore ~ Gender, data = dataset)
print(t_test_result)

# Example ANOVA to compare MathScore among different EthnicGroups
anova_result <- aov(ReadingScore ~ EthnicGroup, data = dataset)
print(summary(anova_result))

# Example correlation between ReadingScore and MathScore
correlation_result <- cor.test(dataset$ReadingScore, dataset$MathScore)
print(correlation_result)

# Compute correlation matrix
cor_matrix <- cor(dataset[numeric_vars], use = "complete.obs")

# Print correlation matrix
cat("\nCorrelation Matrix:\n")
print(cor_matrix)

cat("\nComments on Variable Relationships:\n")
for (i in 1:length(numeric_vars)) {
  for (j in 1:length(numeric_vars)) {
    if (i != j) {
      var1 <- numeric_vars[i]
      var2 <- numeric_vars[j]
      correlation <- cor_matrix[i, j]
      cat("\n", var1, "vs.", var2, ":")
      cat("\nCorrelation Coefficient:", correlation)
      
      # Comment based on correlation strength
      if (abs(correlation) >= 0.7) {
        cat("\nStrong relationship: Highly correlated")
      } else if (abs(correlation) >= 0.4) {
        cat("\nModerate relationship: Moderately correlated")
      } else {
        cat("\nWeak relationship: Weak or no correlation")
      }
    }
  }
}

#Regression Model
library(caret)

set.seed(500)

# Create a partition for training and testing sets
train_index <- createDataPartition(dataset$ReadingScore, p = 0.7, list = FALSE)
train_data <- dataset[train_index, ]
test_data <- dataset[-train_index, ]

# Setup cross-validation control
ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)

# Train Linear Regression model
model_fit_lm <- train(ReadingScore ~ NrSiblings + EthnicGroup + TestPrep + 
                        WklyStudyHours + LunchType + MathScore + WritingScore, 
                      data = train_data, 
                      method = "lm", 
                      trControl = ctrl)

# Make prediction
predictions_lm <- predict(model_fit_lm, newdata = test_data)

# Evaluate the model using RMSE
rmse_lm <- sqrt(mean((test_data$ReadingScore - predictions_lm)^2))

# Print model summary and RMSE
summary(model_fit_lm)
print(paste("RMSE of the Linear Regression model:", rmse_lm))
#-------------------------------------------
#RANDOM FOREST
library(caret)
library(randomForest)

set.seed(500)

# Create a partition for training and testing sets
train_index <- createDataPartition(dataset$ReadingScore, p = 0.7, list = FALSE)
train_data <- dataset[train_index, ]
test_data <- dataset[-train_index, ]

# Setup cross-validation control
ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE, allowParallel = TRUE)

# Train Random Forest Regression model
model_fit_rf <- train(ReadingScore ~ NrSiblings + EthnicGroup + TestPrep + 
                        WklyStudyHours + LunchType + MathScore + WritingScore, 
                      data = train_data, 
                      method = "rf", 
                      trControl = ctrl)

predictions_rf <- predict(model_fit_rf, newdata = test_data)

rmse_rf <- sqrt(mean((test_data$ReadingScore - predictions_rf)^2))

# Print model summary and RMSE
print(model_fit_rf)
print(paste("RMSE of the Random Forest model:", rmse_rf))

#--------------------------------------------
# Classification

library(glmnet)
library(caret)

set.seed(500)

dataset$Above_Average_35_Marks <- ifelse(dataset$Final_Score > 35 , "Pass", "Fail")

index <- createDataPartition(dataset$Above_Average_35_Marks, p = 0.8, list = FALSE)

# Create training and test sets
train_data <- dataset[index, ]
test_data <- dataset[-index, ]

# Define control parameters for cross-validation
ctrl <- trainControl(method = "cv",
                     number = 10,
                     classProbs = TRUE,  
                     summaryFunction = twoClassSummary,
                     search = "grid")

# Define tuning grid for glmnet
alpha_values <- seq(0, 1, by = 0.1)
lambda_values <- 10^seq(-3, 3, by = 0.5)
tuneGrid_glmnet <- expand.grid(.alpha = alpha_values, .lambda = lambda_values)

# Train logistic regression model using glmnet
model_fit_glmnet <- train(Above_Average_35_Marks ~ .,
                          data = train_data,
                          method = "glmnet",
                          trControl = ctrl,
                          tuneGrid = tuneGrid_glmnet)

print(model_fit_glmnet)

k_vals <- c(3, 5, 7, 9)
tuneGrid_knn <- expand.grid(.k = k_vals)

# Train KNN model
model_fit_knn <- train(Above_Average_35_Marks ~ .,
                       data = train_data,
                       method = "knn",
                       trControl = ctrl,
                       tuneGrid = tuneGrid_knn)

# Print the KNN model
print(model_fit_knn)

# Train LDA model
model_fit_lda <- train(Above_Average_35_Marks ~ .,
                       data = train_data,
                       method = "lda",
                       trControl = ctrl)

# Print the LDA model
print(model_fit_lda)

# Evaluate glmnet model
glmnet_pred <- predict(model_fit_glmnet, newdata = test_data)
glmnet_accuracy <- mean(glmnet_pred == test_data$Above_Average_35_Marks)
cat("Logistic Regression Accuracy (glmnet):", glmnet_accuracy, "\n")

# Evaluate KNN model
knn_pred <- predict(model_fit_knn, newdata = test_data)
knn_accuracy <- mean(knn_pred == test_data$Above_Average_35_Marks)
cat("KNN Accuracy:", knn_accuracy, "\n")

# Evaluate LDA model
lda_pred <- predict(model_fit_lda, newdata = test_data)
lda_accuracy <- mean(lda_pred == test_data$Above_Average_35_Marks)
cat("LDA Accuracy:", lda_accuracy, "\n")

