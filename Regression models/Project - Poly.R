# Load necessary libraries
library(readr)
library(tidyr)
library(dplyr)
library(caret)
library(stringr)
library(ggplot2)

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

# Train polynomial regression model using training data
model <- train(view_count ~ poly(likes, 4) + poly(dislikes, 4) + poly(categoryId, 4) + poly(comment_count, 4) + poly(num_tags, 4) + comment_disabled + rating_disabled + poly(title_length, 4) + poly(desc_words, 4),
               data = training_data, method = "lm")

summary(model)
mse <- mean(residuals(model)^2)
rmse_m <- sqrt(mse)
print(paste("RMSE:", rmse_m))
view_count_range <- range(data$view_count)
print(paste("Range of view_count:", view_count_range[2] - view_count_range[1]))

#CA
data_CA <- read_csv("CA_youtube_trending_data.csv")
data_CA <- data_CA %>% 
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

predictions_CA <- predict(model, newdata = data_CA)
rmse_CA <- RMSE(predictions_CA, data_CA$view_count)
rsquared_CA <- cor(predictions_CA, data_CA$view_count)^2
print(paste("RMSE:", rmse_CA))
print(paste("R-squared:", rsquared_CA))

#GB
data_GB <- read_csv("GB_youtube_trending_data.csv")
data_GB <- data_GB %>% 
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

predictions_GB <- predict(model, newdata = data_GB)
rmse_GB <- RMSE(predictions_GB, data_GB$view_count)
rsquared_GB <- cor(predictions_GB, data_GB$view_count)^2
print(paste("RMSE:", rmse_GB))
print(paste("R-squared:", rsquared_GB))

#FR
data_FR <- read_csv("FR_youtube_trending_data.csv")
data_FR <- data_FR %>% 
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

predictions_FR <- predict(model, newdata = data_FR)
rmse_FR <- RMSE(predictions_FR, data_FR$view_count)
rsquared_FR <- cor(predictions_FR, data_FR$view_count)^2
print(paste("RMSE:", rmse_FR))
print(paste("R-squared:", rsquared_FR))



# Sample 1000 observations from testing_data
# sampled_data <- testing_data %>% sample_n(1000)

# Create plot of actual vs. predicted values
predictions <- predict(model, newdata = testing_data)
testing_data$predictions <- predictions
ggplot(data = testing_data, aes(x = view_count, y = predictions)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Actual View Count", y = "Predicted View Count", title = "Actual vs. Predicted View Count")

#CA plot
ca_predictions <- data.frame(predictions_CA, data_CA$view_count)
names(ca_predictions) <- c("Predicted", "Actual")

ggplot(ca_predictions, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Actual View Count", y = "Predicted View Count", title = "Linear Regression Model Performance on CA Dataset")

#GB Plot
gb_predictions <- data.frame(predictions_GB, data_GB$view_count)
names(gb_predictions) <- c("Predicted", "Actual")

ggplot(gb_predictions, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Actual View Count", y = "Predicted View Count", title = "Linear Regression Model Performance on GB Dataset")

#FR Plot
fr_predictions <- data.frame(predictions_FR, data_FR$view_count)
names(fr_predictions) <- c("Predicted", "Actual")

ggplot(fr_predictions, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Actual View Count", y = "Predicted View Count", title = "Linear Regression Model Performance on FR Dataset")

