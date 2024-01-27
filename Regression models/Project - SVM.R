# Load necessary libraries
library(readr)
library(tidyr)
library(dplyr)
library(caret)
library(stringr)
library(e1071)

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

# Scale numeric features
training_data_scaled <- training_data %>%
  mutate_if(is.numeric, scale) # scale numeric features

# Train SVM model using training data
model <- svm(view_count ~ likes + dislikes + categoryId + comment_count + num_tags + comment_disabled + rating_disabled + title_length + title_words + desc_length + desc_words, data = training_data_scaled)

# Make predictions on testing data
testing_data_scaled <- testing_data %>%
  mutate_if(is.numeric, scale) # scale numeric features
predictions <- predict(model, testing_data_scaled)
levels(predictions)
levels(testing_data$view_count)

# Evaluate model performance
confusionMatrix(predictions, testing_data$view_count)
