# Load necessary libraries
library(readr)
library(tidyr)
library(dplyr)
library(caret)
library(stringr)
library(ggplot2)
library(lubridate)
library(tidytext)
library(textdata)

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
         categoryId = as.factor(categoryId),
         year = year(publishedAt),
         month = month(publishedAt),
         day = day(publishedAt),
         week = week(publishedAt))


# Split data into training and testing sets
set.seed(12)
training_samples <- createDataPartition(data$view_count, p = 0.8, list = FALSE)
training_data <- data[training_samples, ]
testing_data <- data[-training_samples, ]

# Train linear regression model using training data
model <- train(view_count ~ likes + dislikes + categoryId + comment_count + num_tags + comment_disabled + rating_disabled + title_length + title_words + desc_length + desc_words + year + month + day + week,
               data = training_data, method = "lm")

summary(model)
mse <- mean(residuals(model)^2)
rmse_m <- sqrt(mse)
print(paste("RMSE:", rmse_m))
view_count_range <- range(data$view_count)
print(paste("Range of view_count:", view_count_range[2] - view_count_range[1]))

#US
predictions_US = predict(model, newdata = testing_data)
rmse_US <- RMSE(predictions_US, testing_data$view_count)
rsquared_US <- cor(predictions_US, testing_data$view_count)^2
print(paste("RMSE:", rmse_US))
print(paste("R-squared:", rsquared_US))

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
         categoryId = as.factor(categoryId),
         year = year(publishedAt),
         month = month(publishedAt),
         day = day(publishedAt),
         week = week(publishedAt))

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
         categoryId = as.factor(categoryId),
         year = year(publishedAt),
         month = month(publishedAt),
         day = day(publishedAt),
         week = week(publishedAt))

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
         categoryId = as.factor(categoryId),
         year = year(publishedAt),
         month = month(publishedAt),
         day = day(publishedAt),
         week = week(publishedAt))

predictions_FR <- predict(model, newdata = data_FR)
rmse_FR <- RMSE(predictions_FR, data_FR$view_count)
rsquared_FR <- cor(predictions_FR, data_FR$view_count)^2
print(paste("RMSE:", rmse_FR))
print(paste("R-squared:", rsquared_FR))

#MX
data_MX <- read_csv("MX_youtube_trending_data.csv")
data_MX <- data_MX %>% 
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
         categoryId = as.factor(categoryId),
         year = year(publishedAt),
         month = month(publishedAt),
         day = day(publishedAt),
         week = week(publishedAt))

predictions_MX <- predict(model, newdata = data_MX)
rmse_MX <- RMSE(predictions_MX, data_MX$view_count)
rsquared_MX <- cor(predictions_MX, data_MX$view_count)^2
print(paste("RMSE:", rmse_MX))
print(paste("R-squared:", rsquared_MX))

#IN
data_IN <- read_csv("IN_youtube_trending_data.csv")
data_IN <- data_IN %>% 
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
         categoryId = as.factor(categoryId),
         year = year(publishedAt),
         month = month(publishedAt),
         day = day(publishedAt),
         week = week(publishedAt))

predictions_IN <- predict(model, newdata = data_IN)
rmse_IN <- RMSE(predictions_IN, data_IN$view_count)
rsquared_IN <- cor(predictions_IN, data_IN$view_count)^2
print(paste("RMSE:", rmse_IN))
print(paste("R-squared:", rsquared_IN))

#JP
data_JP <- read_csv("JP_youtube_trending_data.csv")
data_JP <- data_JP %>% 
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
         categoryId = as.factor(categoryId),
         year = year(publishedAt),
         month = month(publishedAt),
         day = day(publishedAt),
         week = week(publishedAt))

predictions_JP <- predict(model, newdata = data_JP)
rmse_JP <- RMSE(predictions_JP, data_JP$view_count)
rsquared_JP <- cor(predictions_JP, data_JP$view_count)^2
print(paste("RMSE:", rmse_JP))
print(paste("R-squared:", rsquared_JP))

#KR
data_KR <- read_csv("KR_youtube_trending_data.csv")
data_KR <- data_KR %>% 
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
         categoryId = as.factor(categoryId),
         year = year(publishedAt),
         month = month(publishedAt),
         day = day(publishedAt),
         week = week(publishedAt))

predictions_KR <- predict(model, newdata = data_KR)
rmse_KR <- RMSE(predictions_KR, data_KR$view_count)
rsquared_KR <- cor(predictions_KR, data_KR$view_count)^2
print(paste("RMSE:", rmse_KR))
print(paste("R-squared:", rsquared_KR))




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

#MX Plot
mx_predictions <- data.frame(predictions_MX, data_MX$view_count)
names(mx_predictions) <- c("Predicted", "Actual")

ggplot(mx_predictions, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Actual View Count", y = "Predicted View Count", title = "Linear Regression Model Performance on MX Dataset")

#IN Plot
in_predictions <- data.frame(predictions_IN, data_IN$view_count)
names(in_predictions) <- c("Predicted", "Actual")

ggplot(in_predictions, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Actual View Count", y = "Predicted View Count", title = "Linear Regression Model Performance on IN Dataset")

#JP Plot
jp_predictions <- data.frame(predictions_JP, data_JP$view_count)
names(jp_predictions) <- c("Predicted", "Actual")

ggplot(jp_predictions, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Actual View Count", y = "Predicted View Count", title = "Linear Regression Model Performance on JP Dataset")

#KR Plot
kr_predictions <- data.frame(predictions_KR, data_KR$view_count)
names(kr_predictions) <- c("Predicted", "Actual")

ggplot(kr_predictions, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Actual View Count", y = "Predicted View Count", title = "Linear Regression Model Performance on KR Dataset")



ggplot(data, aes(x = comment_count, y = view_count)) +
  geom_point() +
  labs(x = "comment_count", y = "View count") +
  ggtitle("View count vs comment_count")


# Make prediction using trained model
new_prediction <- predict(model, newdata = new_data)



#create CSV files
write.csv(data.frame(data_source = "US",predictions_US,testing_data), "data_US.csv", row.names = FALSE)
write.csv(data.frame(data_source = "CA",predictions_CA,data_CA), "data_CA.csv", row.names = FALSE)
write.csv(data.frame(data_source = "FR",predictions_FR,data_FR), "data_FR.csv", row.names = FALSE)
write.csv(data.frame(data_source = "GB",predictions_GB,data_GB), "data_GB.csv", row.names = FALSE)
write.csv(data.frame(data_source = "JP",predictions_JP,data_JP), "data_JP.csv", row.names = FALSE)
write.csv(data.frame(data_source = "KR",predictions_KR,data_KR), "data_KR.csv", row.names = FALSE)
write.csv(data.frame(data_source = "MX",predictions_MX,data_MX), "data_MX.csv", row.names = FALSE)

