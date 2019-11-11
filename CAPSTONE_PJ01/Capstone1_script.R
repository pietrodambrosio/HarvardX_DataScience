###########################################################################
#--------------------------------------------------------------------------
# HARVARD PROFESSIONAL DATA SCIENCE SPECIALIZATION
# CAPSTONE 1 - MOVIELENS PROJECT
# PIETRO D'AMBROSIO  pietro.dambrosio@ordingsa.it
# https://github.com/pietrodambrosio/HarvardX_DataScience
#--------------------------------------------------------------------------
###########################################################################

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
# STEP1 - CREATING TRAINING AND VALIDATION DATA
# Dataset loading script (as provided in the project requirements)
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------

################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

# set.seed(1, sample.kind="Rounding")
set.seed(1)
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
# STEP2 - COMPLETING ENVIRONMENT AND STARTING PRE-ELABORATION
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------

# We save the edx e validation dataset so we can use it later 
save(edx,file="save_edx.Rdata")
save(validation,file="save_validation.Rdata")


# Define additional libraries 
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(lattice)) install.packages("lattice", repos = "http://cran.us.r-project.org")


# Define the RMSE (Root Mean Square Error) function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Now we create a test partition for tune the model trained without use the verification set.
# This partition will be 10% of the training set. We will use semi_join to be sure that
# We will use the semi_join function to make sure that all the userIds and movieIds 
# in the test dataset are also included in the train dataset
set.seed(1)
index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train <- edx[-index,]
test <- edx[index,] %>% 
        semi_join(train, by = "movieId") %>% 
        semi_join(train, by = "userId")

# The rows removed from test set will be inserted into train set
rows_rem <- edx[index,] %>% anti_join(test) 
train <- rbind(train, rows_rem)

# remove the unused objects
rm(rows_rem, index) 

# Save the train and test dataset   
save(train,file="save_train.Rdata")
save(test,file="save_test.Rdata")



#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
# STEP3 - MODELING
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------

#############################
# MOD. 1 - THE SIMPLEST MODEL
# just the average of rating
#############################

# This is the start point for every further step. It's give us an idea of
# how much our results will improve
mu <- mean(train$rating)
rmse   <- RMSE(mu,test$rating)

# Create a rmse tab to collect all results 
rmse_tab <- data_frame(method = "Model 1 - Simple average og rating", RMSE = rmse)
rmse_tab


#############################################
# MOD. 2 - ADD MOVIE EFFECT
# some movies are rated higher than others
#############################################

movie_avg  <- train %>% group_by(movieId) %>% summarise(b_i = mean(rating - mu))

predicted <- mu + test %>% left_join(movie_avg,by="movieId") %>% pull(b_i)
rmse <- RMSE(predicted, test$rating)

rmse_tab <- rbind(rmse_tab,c(method = "Model 2 - Add movie effect", RMSE = rmse))
rmse_tab


############################################################
# MOD. 3 - ADD USER EFFECT
# some users tend to give a higher or lower average rating
############################################################

user_avg  <- train %>% left_join(movie_avg,by="movieId") %>%
             group_by(userId) %>% summarise(b_u = mean(rating - mu - b_i))

predicted <- test %>% left_join(movie_avg,by="movieId") %>% 
                      left_join(user_avg,by="userId") %>%
                      mutate(pred = mu + b_i + b_u) %>%
                      pull(pred)
rmse <- RMSE(predicted, test$rating)

rmse_tab <- rbind(rmse_tab,c(method = "Model 3 - Add user effect", RMSE = rmse))
rmse_tab


############################################
# MOD. 4 - ADD YEAR EFFECT
# the rating depends on how old the film is
############################################

train$year <- as.numeric(str_sub(train$title,-5,-2))
test$year  <- as.numeric(str_sub(test$title,-5,-2))

year_avg  <- train %>% left_join(movie_avg,by="movieId") %>%
                       left_join(user_avg,by="userId") %>%
                       group_by(year) %>% 
                       summarise(b_y = mean(rating - mu - b_i - b_u))

predicted <- test %>% left_join(movie_avg,by="movieId") %>% 
                      left_join(user_avg,by="userId") %>%
                      left_join(year_avg,by="year") %>%
                      mutate(pred = mu + b_i + b_u + b_y) %>%
                      pull(pred)
rmse <- RMSE(predicted, test$rating)

rmse_tab <- rbind(rmse_tab,c(method = "Model 4 - Add year effect", RMSE = rmse))
rmse_tab


############################################
# MOD. 5 - ADD GENRE EFFECT
# the rating also depends on genre of movie
############################################

# for examine the genres we'll split the "genres" field in multiple rows
train_split <- train %>%separate_rows(genres, sep = "\\|")
test_split  <- test  %>%separate_rows(genres, sep = "\\|")

# this processing will slightly change the average due to the duplication of the lines by genre
genre_avg  <- train_split %>% left_join(movie_avg,by="movieId") %>%
                        left_join(user_avg,by="userId") %>%
                        left_join(year_avg, by = 'year') %>%
                        group_by(genres) %>% 
                        summarise(b_g = mean(rating - mu - b_i - b_u - b_y))

predicted <- test_split %>% left_join(movie_avg,by="movieId") %>% 
                      left_join(user_avg,by="userId") %>%
                      left_join(year_avg,by="year") %>%
                      left_join(genre_avg, by = 'genres') %>%
                      mutate(pred = mu + b_i + b_u + b_y + b_g) %>%
                      pull(pred)
rmse <- RMSE(predicted, test_split$rating)

rmse_tab <- rbind(rmse_tab,c(method = "Model 5 - Add genre effect", RMSE = rmse))
rmse_tab


##############################################################
# MOD. 6 - APPLY REGULARIZATION
# We penalize large estimates formed with low number of ratings
##############################################################

# we'll use cross validation to found the right lambda value
lambdas <- seq(0, 20, 1)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_split$rating)
  
  b_i <- train_split %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_split %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_y <- train_split %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    group_by(year) %>%
    summarize(b_y = sum(rating - mu - b_i - b_u)/(n()+l))
  
  b_g <- train_split %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(b_y, by = 'year') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u - b_y)/(n()+l))
  
  predicted <- test_split %>% 
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(b_y, by = 'year') %>%
    left_join(b_g, by = 'genres') %>%
    mutate(pred = mu + b_i + b_u + b_y + b_g) %>% 
    pull(pred)
  
  return(RMSE(predicted,test_split$rating))
})

save(lambdas,rmses,file="save_lambdas_rmses.Rdata")
qplot(lambdas, rmses) 

lambda <- lambdas[which.min(rmses)]
rmse = min(rmses)

rmse_tab <- rbind(rmse_tab,c(method = "Model 6 - Add regularization", RMSE = rmse))
rmse_tab

# save image for restart without re-execute previous steps
save.image(file='save_all_env.RData')


###############################################################################
# MOD. 7 - MATRIX FACTORIZATION
# we will try to intercept the evaluation model of a group of similar films
###############################################################################

# movie effect on train set
b_i <- train %>% 
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

# user effect on train set
b_u <- train %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - b_i - mu))

# compute residuals (rating - mean - movie eff - user eff) on train set
train <- train %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(res = rating - mu - b_i - b_u)

# compute residuals on test set
test <- test %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(res = rating - mu - b_i - b_u)

# we'll use recosystem package
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")

# we'll create two recosystem object for train e test set
# user_index is an integer vector giving the user indices of rating scores 
# item_index is an integer vector giving the item indices of rating scores (movie)
# rating is a numeric vector of the observed entries in the rating matrix.
# index1 = TRUE indicates that index start with 1 (if FALSE start with 0)
train_data <- data_memory(user_index = train$userId, item_index = train$movieId, 
                          rating = train$res, index1 = T)
test_data  <- data_memory(user_index = test$userId, item_index = test$movieId, index1 = T)

# create a model object
recommender <- Reco()

# tune the model parameters
# WARNING !!! please set properly the "nthread" if you use a multicore cpu
set.seed(1) 

res = recommender$tune(
   train_data,
   opts = list(dim = c(30, 32),
               costp_l1 = 0, 
               costq_l1 = c(0,0.1),
               costp_l2 = c(0,0.01), 
               costq_l2 = c(0,0.1),
               lrate = c(0.05, 0.06), 
               nthread = 1)
   )

# these are the best parameters
opts_tuned = res$min

# save image for restart without re-execute previous steps
save.image(file='save_all_env2.RData')

# Train the model with the best parameters  
set.seed(1) 
suppressWarnings(recommender$train(train_data, opts = opts_tuned))

# we will first predict the residual on test set and store predict in a vector
pred <- recommender$predict(test_data, out_memory())

# then add to residual the mean, the movie factor and the user factor
predicted <- pred + mu + test$b_i + test$b_u
save (predicted,test,file="Matrix_fact_predicted.Rdata")

rmse = RMSE(predicted,test$rating)
## 0.793795

# further adjustments
# we observed that the rating systems don't have ratings > 5 and ratings < 0.5
# then we make these changes
predicted[predicted > 5] <- 5
predicted[predicted < 0.5] <- 0.5

# compute the new rmse
rmse = RMSE(predicted,test$rating)

# add rmse to summary table
rmse_tab <- rbind(rmse_tab,c(method = "Model 7 - Matrix Factorization", RMSE = rmse))
rmse_tab

#######################
# 0.793539295994846
#######################

# the factoring matrix is certainly the best performing model so we will use it to verify the
# RMSE on the validation dataset. to do this we must apply the same previous transformations 
# to the validation dataset

# movies effect on entire edx set
b_i <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

# users effect on entire edx set
b_u <- edx %>%
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - b_i - mu))

# compute residuals on edx set
edx <- edx %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(res = rating - mu - b_i - b_u)

# compute residuals on validation set
validation <- validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(res = rating - mu - b_i - b_u)

# create two recosystem object for train e test set (in this case edx and validation)
edx_data <- data_memory(user_index = edx$userId, item_index = edx$movieId, 
                        rating = edx$res, index1 = T)
validation_data <- data_memory(user_index = validation$userId, item_index = validation$movieId, index1 = T)

# create a model object
recommender <- Reco()

set.seed(1)

# Train the model with the best parameters found previously 
suppressWarnings(recommender$train(edx_data, opts = opts_tuned))

# we will first predict the residual on validation set and store predict in a vector
pred <- recommender$predict(validation_data, out_memory())

# then add to residual the mean, the movie factor and the user factor
predicted <- pred + mu + validation$b_i + validation$b_u

# the rating systems don't have ratings > 5 and ratings < 0.5
predicted[predicted > 5] <- 5
predicted[predicted < 0.5] <- 0.5
save (predicted, validation,file="Matrix_fact_predicted_valid.Rdata")

# compute the rmse
rmse = RMSE(predicted,validation$rating)

# add rmse to table
rmse_tab <- rbind(rmse_tab,c(method = "Model 8 - Matrix Factorization - Final model", RMSE = rmse))
rmse_tab

rmse_final = rmse
save(rmse_final,file="rmse_final.Rdata")
##########
# 0.7907
##########




