##########################################################
# Create edx and final_holdout_test sets 
##########################################################

if (!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if (!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if (!file.exists(dl)) {
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
}

# Unzip the dataset if the ratings file does not exist
ratings_file <- "ml-10M100K/ratings.dat"
if (!file.exists(ratings_file)) {
  unzip(dl, files = ratings_file)
}

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Data Inspection
dim(edx)
sum(edx$rating == 0)
sum(edx$rating == 3)
n_distinct(edx$movieId)
n_distinct(edx$userId)

genre_counts <- edx %>%
  mutate(Drama = str_detect(genres, "Drama"),
         Comedy = str_detect(genres, "Comedy"),
         Thriller = str_detect(genres, "Thriller"),
         Romance = str_detect(genres, "Romance")) %>%
  summarise(Drama = sum(Drama),
            Comedy = sum(Comedy),
            Thriller = sum(Thriller),
            Romance = sum(Romance))
genre_counts
head(edx)

# Filter the dataset for the specified movies
specified_movies <- c("Forrest Gump", "Jurassic Park", "Pulp Fiction", "The Shawshank Redemption", "Speed 2: Cruise Control")

# Function to extract the base title without the year
extract_base_title <- function(title) {
  str_replace(title, " \\(\\d{4}\\)$", "")
}

# Add a new column with the base title
edx <- edx %>%
  mutate(base_title = extract_base_title(title))

# Filter the dataset for the specified movies
ratings_count <- edx %>%
  filter(base_title %in% specified_movies) %>%
  group_by(base_title) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
ratings_count

# Count the occurrences of each rating
rating_counts <- edx %>%
  count(rating) %>%
  arrange(desc(n))

head(rating_counts, 5)

# Separate whole-star and half-star ratings
rating_counts <- rating_counts %>%
  mutate(rating_type = ifelse(rating %% 1 == 0, "whole", "half"))

# Summarize the counts of whole-star and half-star ratings
summary_counts <- rating_counts %>%
  group_by(rating_type) %>%
  summarise(total_count = sum(n))
summary_counts

# Check if half-star ratings are less common than whole-star ratings
half_star_less_common <- summary_counts %>%
  filter(rating_type == "half") %>%
  pull(total_count) < summary_counts %>%
  filter(rating_type == "whole") %>%
  pull(total_count)

half_star_less_common

summary(edx)
glimpse(edx)

# Check for missing values
missing_values <- sapply(edx, function(x) sum(is.na(x)))
missing_values

# Check for duplicates
num_duplicates <- sum(duplicated(edx))
num_duplicates

# Data Cleaning
edx <- edx %>%
  mutate(movieId = as.integer(movieId),
         rating = as.numeric(rating)) %>%
  drop_na() %>%
  distinct()

# Verify changes
dim(edx)
summary(edx)
glimpse(edx)

# Check the number of unique users and unique movies
edx %>%
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

# Filter users who have rated at least 50 movies
user_ratings <- edx %>%
  group_by(userId) %>%
  summarize(count = n()) %>%
  filter(count >= 50)

# Filter movies that have been rated at least 50 times
movie_ratings <- edx %>%
  group_by(movieId) %>%
  summarize(count = n()) %>%
  filter(count >= 50)

# Reduce the edx dataset
edx_filtered <- edx %>%
  semi_join(user_ratings, by = "userId") %>%
  semi_join(movie_ratings, by = "movieId")

# Verify the reduction
edx_filtered %>%
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

# Distribution of Ratings
ggplot(edx_filtered, aes(x = rating)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "Distribution of Ratings", x = "Rating", y = "Count")

# Ratings per Movie
ratings_per_movie <- edx_filtered %>%
  group_by(movieId, title) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

ggplot(ratings_per_movie, aes(x = count)) +
  geom_histogram(binwidth = 50, fill = "green", color = "black") +
  labs(title = "Number of Ratings per Movie", x = "Number of Ratings", y = "Count")

# Ratings per User
ratings_per_user <- edx_filtered %>%
  group_by(userId) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

ggplot(ratings_per_user, aes(x = count)) +
  geom_histogram(binwidth = 20, fill = "purple", color = "black") +
  labs(title = "Number of Ratings per User", x = "Number of Ratings", y = "Count")

# Genre Analysis
# Split genres into separate rows
genres_split <- edx_filtered %>%
  separate_rows(genres, sep = "\\|")

# Count ratings for each genre
ratings_per_genre <- genres_split %>%
  group_by(genres) %>%
  summarise(count = n(), average_rating = mean(rating)) %>%
  arrange(desc(count))

ggplot(ratings_per_genre, aes(x = reorder(genres, -count), y = count)) +
  geom_bar(stat = "identity", fill = "orange", color = "black") +
  labs(title = "Number of Ratings per Genre", x = "Genre", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot average rating per genre
ggplot(ratings_per_genre, aes(x = reorder(genres, -average_rating), y = average_rating)) +
  geom_bar(stat = "identity", fill = "cyan", color = "black") +
  labs(title = "Average Rating per Genre", x = "Genre", y = "Average Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Split the filtered edx data into training and validation sets
set.seed(1, sample.kind="Rounding")
train_index <- createDataPartition(y = edx_filtered$rating, times = 1, p = 0.8, list = FALSE)
train_set <- edx_filtered[train_index,]
validation_set <- edx_filtered[-train_index,]

# Verify the splits
train_set %>%
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

validation_set %>%
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

# Calculate the average of all ratings (mu)
mu <- mean(train_set$rating)

# Predict the mean rating for the validation set
baseline_predictions <- rep(mu, nrow(validation_set))

# Calculate RMSE for the baseline model
baseline_rmse <- RMSE(validation_set$rating, baseline_predictions)

# Create a results table with this naive approach
rmse_results <- tibble(method = "Baseline Model: Just the average", RMSE = baseline_rmse)
rmse_results

# Calculate the average rating for each movie (b_i)
movie_avgs <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

# Predict ratings in the validation set using the movie effect model
movie_effect_predicted_ratings <- validation_set %>%
  left_join(movie_avgs, by = 'movieId') %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)

# Calculate RMSE for the movie effect model
movie_effect_rmse <- RMSE(movie_effect_predicted_ratings, validation_set$rating)

# Add the movie effect model results to the results table
rmse_results <- rmse_results %>%
  add_row(method = "Movie Effect Model", RMSE = movie_effect_rmse)
rmse_results

# Calculate the average rating for each user (b_u)
user_avgs <- train_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# Predict ratings in the validation set using the user effect model
user_effect_predicted_ratings <- validation_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

# Calculate RMSE for the user effect model
user_effect_rmse <- RMSE(user_effect_predicted_ratings, validation_set$rating)

# Add the user effect model results to the results table
rmse_results <- rmse_results %>%
  add_row(method = "User Effect Model", RMSE = user_effect_rmse)
rmse_results

# Define a range of lambda values to test
lambdas <- seq(0, 10, 0.25)

# Function to calculate RMSE for a given lambda
calculate_rmse <- function(lambda) {
  mu <- mean(train_set$rating)
  
  # Regularized movie averages
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu) / (n() + lambda))
  
  # Regularized user averages
  b_u <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu) / (n() + lambda))
  
  # Predict ratings in the validation set
  predicted_ratings <- validation_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation_set$rating))
}

# Calculate RMSE for each lambda
rmses <- sapply(lambdas, calculate_rmse)

# Plot the RMSE values against lambda
qplot(lambdas, rmses, geom = "line") +
  labs(title = "RMSE vs. Lambda", x = "Lambda", y = "RMSE")

# Find the best lambda
best_lambda <- lambdas[which.min(rmses)]
print(paste("Best lambda:", best_lambda))

# Recalculate the model using the best lambda
mu <- mean(train_set$rating)

# Regularized movie averages
b_i <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu) / (n() + best_lambda))

# Regularized user averages
b_u <- train_set %>%
  left_join(b_i, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu) / (n() + best_lambda))

# Predict ratings in the validation set using the best lambda
reg_predictions <- validation_set %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

# Calculate RMSE for the regularized model using the best lambda
reg_rmse <- RMSE(reg_predictions, validation_set$rating)

# Add the regularized model results to the results table
rmse_results <- rmse_results %>%
  add_row(method = "Regularized Movie + User Effect Model", RMSE = reg_rmse)
rmse_results

# Install and load the recosystem package if not already installed
if (!require(recosystem)) install.packages("recosystem")
library(recosystem)

# Prepare data in recosystem format
train_data <- data_memory(train_set$userId, train_set$movieId, train_set$rating)
validation_data <- data_memory(validation_set$userId, validation_set$movieId)

# Create the Reco model object
r <- Reco()

# Train the model
r$train(train_data, opts = list(dim = 30, lrate = 0.1, costp_l2 = 0.1, costq_l2 = 0.1, niter = 20, nthread = 2))

# Predict ratings for the validation set
predicted_ratings <- r$predict(validation_data)

# Calculate RMSE for the matrix factorization model
mf_rmse <- RMSE(predicted_ratings, validation_set$rating)

# Add the matrix factorization model results to the results table
rmse_results <- rmse_results %>%
  add_row(method = "Matrix Factorization", RMSE = mf_rmse)
rmse_results

# Regularized movie averages (b_i)
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + l))
  b_u <- train_set %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n() + l))
  predicted_ratings <- validation_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  return(RMSE(predicted_ratings, validation_set$rating))
})

lambda <- lambdas[which.min(rmses)]

# Calculate regularized movie averages (b_i)
movie_reg_avgs <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu) / (n() + lambda))

# Calculate regularized user averages (b_u)
user_reg_avgs <- train_set %>%
  left_join(movie_reg_avgs, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i) / (n() + lambda))

# Predict ratings using the regularized model
reg_predictions <- validation_set %>%
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(user_reg_avgs, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)


# Prepare data in recosystem format
train_data <- data_memory(train_set$userId, train_set$movieId, train_set$rating)
validation_data <- data_memory(validation_set$userId, validation_set$movieId)

# Create the Reco model object
r <- Reco()

# Train the model
r$train(train_data, opts = list(dim = 30, lrate = 0.1, costp_l2 = 0.1, costq_l2 = 0.1, niter = 20, nthread = 2))

# Predict ratings for the validation set
mf_predictions <- r$predict(validation_data)

# Define weights for blending
w1 <- 0.5  # Weight for regularized model
w2 <- 0.5  # Weight for matrix factorization model

# Calculate blended predictions
blended_predictions <- w1 * reg_predictions + w2 * mf_predictions

# Calculate RMSE for the hybrid model
hybrid_rmse <- RMSE(blended_predictions, validation_set$rating)

# Add the hybrid model results to the results table
rmse_results <- rmse_results %>%
  add_row(method = "Hybrid Model", RMSE = hybrid_rmse)
rmse_results

# Select the model with the lowest RMSE
best_model <- rmse_results %>%
  filter(RMSE == min(RMSE)) %>%
  pull(method)
print(paste("Best performing model:", best_model))

# Define the number of folds
k <- 5

# Create folds
set.seed(1)
folds <- createFolds(edx$rating, k = k, list = TRUE, returnTrain = TRUE)

# Function to calculate RMSE for each fold
cv_rmse <- function(fold, train_data) {
  # Prepare training and validation sets for the fold
  train_fold <- edx[fold, ]
  validation_fold <- edx[-fold, ]
  
  # Prepare data in recosystem format
  train_data <- data_memory(train_fold$userId, train_fold$movieId, train_fold$rating)
  validation_data <- data_memory(validation_fold$userId, validation_fold$movieId)
  
  # Create the Reco model object
  r <- Reco()
  
  # Train the model
  r$train(train_data, opts = list(dim = 30, lrate = 0.1, costp_l2 = 0.1, costq_l2 = 0.1, niter = 20, nthread = 2))
  
  # Predict ratings for the validation fold
  predicted_ratings <- r$predict(validation_data)
  
  # Calculate RMSE for the fold
  rmse <- RMSE(predicted_ratings, validation_fold$rating)
  
  return(rmse)
}

# Perform k-fold cross-validation
cv_rmses <- sapply(folds, cv_rmse)

# Calculate the average RMSE from cross-validation
cv_rmse_mean <- mean(cv_rmses)

# Print the cross-validation results
print(paste("Average RMSE from cross-validation:", cv_rmse_mean))

# Prepare data in recosystem format
train_data <- data_memory(edx$userId, edx$movieId, edx$rating)

# Create the Reco model object
r <- Reco()

# Train the model on the entire edx dataset
r$train(train_data, opts = list(dim = 30, lrate = 0.1, costp_l2 = 0.1, costq_l2 = 0.1, niter = 20, nthread = 2))

# Prepare final_holdout_test data in recosystem format
holdout_data <- data_memory(final_holdout_test$userId, final_holdout_test$movieId)

# Predict ratings for the final_holdout_test set
final_predictions <- r$predict(holdout_data)

# Calculate RMSE for the final model on the final_holdout_test set
final_rmse <- RMSE(final_predictions, final_holdout_test$rating)

# Add RMSE for the final model on the results to the results table
rmse_results <- rmse_results %>%
  add_row(method = "Matrix Factorization on final_holdout_test set", RMSE = final_rmse)
rmse_results


