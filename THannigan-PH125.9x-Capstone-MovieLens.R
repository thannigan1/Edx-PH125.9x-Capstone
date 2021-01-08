##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")


# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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

# Add additional needed packages and libraries

install.packages("ggplot2")

library(ggplot2)

# Identify movieYr, ratingYr, and rating delay and add to edx and validation

edx <- mutate(edx, movieYr =as.numeric(str_sub(title,-5, -2)))
edx <- mutate(edx, ratingYr = year(as_datetime(timestamp)))
edx <- mutate(edx, delay = ratingYr - movieYr)

head(edx)

validation <- mutate(validation,movieYr=as.numeric(str_sub(title,-5, -2)))
validation <- mutate(validation, ratingYr = year(as_datetime(timestamp)))
validation <- mutate(validation, delay = ratingYr - movieYr)

head(validation)


# Initial visualizations

# Movie rating variation visualized

edx %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.25, color = "darkblue", fill = "lightblue") +
  scale_y_continuous(labels = scales::comma) +
  stat_count(aes(y=..count..,label=..count..),geom="text",vjust=-1) +
  ggtitle("Rating Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

edx %>% group_by(movieId) %>% summarize(mean=mean(rating)) %>% 
  ggplot(aes(x=mean))+
  geom_density(color="darkblue", fill="lightblue") +
  ggtitle("Movie Mean Rating Density") +
  theme(plot.title = element_text(hjust = 0.5))


# User rating variation visualized


edx %>% group_by(movieId) %>% filter(n() >= 100) %>%
  summarize(sd=sd(rating)) %>% 
  ggplot(aes(x=sd))+
  geom_density(color="sienna", fill="sienna1") +
  ggtitle("Movie Rating Standard Deviation Density (>100 reviews/movie)") +
  theme(plot.title = element_text(hjust = 0.5))


# Genre impact visualized


genre_graph <- edx %>%
  group_by(genres) %>%
  filter(n() >= 30000 & str_count(genres,"\\|")<2) %>%
  summarize(mean = mean(rating)) %>%
  arrange(., mean)

genre_graph %>% ggplot(aes(mean,reorder(genres,-mean))) + 
  geom_point(color = "darkolivegreen4") +
  ggtitle("Mean movie ratings for specific Genres (min 30000 reviews, max 2 identified Genres)") +
  theme(plot.title = element_text(hjust = 0.5))


# Rating delay impact visualized


edx %>%
  group_by(delay) %>%
  filter(n() >= 1000) %>%
  summarize(b_d = mean(rating)) %>%
  ggplot(aes(delay,b_d)) +
  geom_point() +
  geom_smooth(method = 'loess', formula = y ~ x, color = "purple") +
  ylab("Mean Rating") +
  xlab("Delay (Years)") +
  ggtitle("Mean movie ratings by rating delay (min 1000 entries for delay year)")+
  theme(plot.title = element_text(hjust = 0.5))


# Identify naive baseline by fitting rmse using mean of ratings from "edx" and testing on "validation"


mu <- mean(edx$rating)
mu

naive_rmse <- RMSE(validation$rating, mu)
naive_rmse

rmse_results <- tibble(method = "Average movie rating model", RMSE = naive_rmse)
rmse_results %>% knitr::kable()


# Augment the baseline by factoring in the movie impact


movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

movie_avgs %>% qplot(b_i, geom ="histogram", bins = 25, data = ., fill = I("lightblue"), 
                     color = I("darkblue"), ylab = "Number of movies", main = "Histogram of movies by b_i") +
  theme(plot.title = element_text(hjust = 0.5))

predicted_ratings <- mu +  validation %>%
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)

model_M_rmse <- RMSE(predicted_ratings, validation$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie effect model",  
                                     RMSE = model_M_rmse ))

rmse_results %>% knitr::kable()


# Augment the movie effect model by factoring in the user impact


user_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

user_avgs%>% qplot(b_u, geom ="histogram", bins = 25, data = ., color = I("sienna"), 
                   fill = I("sienna1"), main = "Histogram of Frequency by b_u") +
  theme(plot.title = element_text(hjust = 0.5))

predicted_ratings <- validation %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

model_U_rmse <- RMSE(predicted_ratings, validation$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie and user effect model",  
                                     RMSE = model_U_rmse))

rmse_results %>% knitr::kable()


# Augment the movie and user effect model by factoring in the genre impact


genre_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u))

genre_avgs %>% qplot(b_g, geom ="histogram", bins = 25, data = ., color = I("darkolivegreen4"), 
                     fill = I("darkolivegreen2"), main = "Histogram of Frequency by b_g") +
  theme(plot.title = element_text(hjust = 0.5))

predicted_ratings <- validation%>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)

model_G_rmse <- RMSE(predicted_ratings, validation$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie and user and genre effect model",  
                                     RMSE = model_G_rmse))

rmse_results %>% knitr::kable()


# Augment the movie and user and genre effect model by factoring in the delay impact


delay_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genres_avgs, by='genres') %>%
  group_by(delay) %>%
  summarize(b_d = mean(rating - mu - b_i - b_u - b_g))

delay_avgs %>% qplot(b_d, geom ="histogram", bins = 25, data = ., color = I("purple"), 
                     fill = I("lavender"), main = "Histogram of Frequency by b_d") +
  theme(plot.title = element_text(hjust = 0.5))

predicted_ratings <- validation%>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genres_avgs, by='genres') %>%
  left_join(delay_avgs, by='delay') %>%
  mutate(pred = mu + b_i + b_u + b_d + b_g) %>%
  pull(pred)

model_D_rmse <- RMSE(predicted_ratings, validation$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie and user and genre and delay effect model",  
                                     RMSE = model_D_rmse))

rmse_results %>% knitr::kable()


# Utilize regularization to further refine the models

lambdas <- seq(3.5, 7, 0.1)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_g <- edx %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_u - b_i - mu)/(n()+l))
  
  b_d <- edx %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    group_by(delay) %>%
    summarize(b_d = sum(rating - b_g - b_u - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    left_join(b_d, by="delay") %>%
    mutate(pred = mu + b_i + b_u + b_g + b_d) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
})


# Plot rmses vs lambdas to select the optimal lambda                                                             

qplot(lambdas, rmses, main = "Identifying Optimal lambda") +
  theme(plot.title = element_text(hjust = 0.5))  

lambdas[which.min(rmses)]

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized movie/user/genre/delay effect model",  
                                     RMSE = min(rmses)))

# Final Results

rmse_results %>% knitr::kable()


# Citations

#MovieLens 10M Dataset:  https://grouplens.org/datasets/movielens/10m/

#Acadamy awards data:  https://www.filmsite.org/ 

#Rotten Tomatoes data:  https://www.rottentomatoes.com/m/ballistic_ecks_vs_sever

#Initial Code provided as part of HarvardX PH125.9x Capstone Course: https://www.edx.org/professional-certificate/harvardx-data-science

#Textbook:  “Introduction to Data Science”, by Rafael A. Irizarry: https://rafalab.github.io/dsbook/
  



