# Load necessary libraries
library(readr)
library(tidyr)
library(dplyr)
library(caret)
library(stringr)
library(class)

# Load data from CSV file
data <- read_csv("us_youtube_trending_data.csv")

# Preprocess data
data <- data %>% 
  #select(-video_id) %>% # Remove ID column
  mutate_if(is.character, as.factor) %>% # Convert character columns to factors
  drop_na() %>% # Remove any rows with missing data
  mutate(num_tags = ifelse(tags == "None", 0, str_count(tags, "\\|") + 1),
         comment_disabled = ifelse(comments_disabled != "FALSE", 1, 0),
         rating_disabled = ifelse(ratings_disabled != "FALSE", 1, 0),
         title = as.character(title), # convert to character
         title_length = nchar(title),
         title_words = str_count(title, "\\w+"), # count number of words in title
         description = as.character(description), # convert to character
         desc_length = nchar(description),
         desc_words = str_count(description, "\\w+"), # count number of words in description
         categoryId = as.factor(categoryId))

# Split data into training and testing sets
set.seed(12)
training_samples <- createDataPartition(data$view_count, p = 0.8, list = FALSE)
training_data <- data[training_samples, ]
testing_data <- data[-training_samples, ]

# Limit factors to use in the KNN model
factors <- c("likes", "dislikes", "categoryId", "comment_count", "num_tags", "comment_disabled", "rating_disabled", "title_length", "title_words", "desc_length", "desc_words")

# Scale numeric features in the training and testing data
training_data_scaled <- training_data %>%
  select(factors) %>%
  mutate_if(is.numeric, scale) # scale numeric features

testing_data_scaled <- testing_data %>%
  select(factors) %>%
  mutate_if(is.numeric, scale) # scale numeric features

# Train KNN model using training data
k <- 33 # number of neighbors to consider
model <- knn(train = training_data_scaled, test = testing_data_scaled, cl = training_data$view_count, k = k)

# Create predicted view count vector
predicted_view_counts <- as.numeric(as.character(model))

# Create binary vectors indicating whether actual and predicted view counts are above or below the median
actual_above_median <- ifelse(testing_data$view_count > median(testing_data$view_count), 1, 0)
predicted_above_median <- ifelse(predicted_view_counts > median(testing_data$view_count), 1, 0)

# Create 2x2 confusion matrix
confusion_matrix <- table(predicted_above_median, actual_above_median)
colnames(confusion_matrix) <- c("Actual Below Median", "Actual Above Median")
rownames(confusion_matrix) <- c("Predicted Below Median", "Predicted Above Median")

# Print confusion matrix and accuracy
print(confusion_matrix)

TP <- confusion_matrix[2, 2]
TN <- confusion_matrix[1, 1]
FP <- confusion_matrix[1, 2]
FN <- confusion_matrix[2, 1]

accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1 <- 2 * precision * recall / (precision + recall)

cat("Accuracy:", round(accuracy, 2), "\n")
cat("Precision:", round(precision, 2), "\n")
cat("Recall:", round(recall, 2), "\n")
cat("F1-score:", round(f1, 2))


# Train KNN model using training data and determine optimal k value using elbow method
max_k <- 60
k_values <- 1:max_k
errors <- data.frame(k = k_values, error = numeric(length(k_values)))

for (k in k_values) {
  model <- knn(train = training_data_scaled, test = testing_data_scaled, cl = training_data$view_count, k = k)
  predicted_view_counts <- as.numeric(as.character(model))
  errors[k, "error"] <- mean(abs(predicted_view_counts - testing_data$view_count))
}

plot(errors$k, errors$error, type = "l", xlab = "k", ylab = "Mean Absolute Error")