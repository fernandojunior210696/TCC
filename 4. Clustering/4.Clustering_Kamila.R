# 4. Clustering Kamila

# settings
setwd("~/Desktop/TCC")

# load required packages
library(tidyverse)
library(magrittr)
library(lubridate)
library(kamila)
library(zoo)
library(psych)
# clustering
# library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(caret)

# read databases
all_songs <- read_csv('2.Datasets/features.csv')
most_popular <- read_csv('2.Datasets/most_popular.csv')
clustering_features <- read_csv('2.Datasets/clusterig_features.csv')

# encode factors features
all_songs$time_signature %<>% as.factor()
all_songs$mode %<>% as.factor()
all_songs$key %<>% as.factor()

most_popular$time_signature %<>% as.factor()
most_popular$mode %<>% as.factor()
most_popular$key %<>% as.factor()

# encode date features
all_songs$track.album.release_date %<>% as.Date()

most_popular$date %<>% as.Date()
most_popular$track.album.release_date %<>% as.Date()

most_popular$ano_mes <- as.yearmon(most_popular$date)
most_popular$ano_mes %<>% factor(ordered = TRUE)

# # create feature of days since release
# all_songs$days_since_release <- date('2020-03-14') - all_songs$track.album.release_date
# most_popular$days_since_release <- date('2020-03-14') - most_popular$track.album.release_date
# 
# # encode days since release as numeric
# all_songs$days_since_release %<>% as.integer()
# most_popular$days_since_release %<>% as.integer()

## we are going to test some approachs in order to cluster the popular songs

# 1. Let's consider all songs that were popular at any moment 

# dataframe filtered
songs_pop <- most_popular %>%
  select(-c(date, mes, ano, ano_mes, Position)) %>% 
  unique() %>%
  drop_na()

# join clustering features
df <- songs_pop %>% left_join(clustering_features, by = 'id')

# create dataframe with continuous features
MP_continuous <- df %>%
  select(-c('id', 'track.popularity','track.album.release_date',
            'track.album.release_date_precision', 'key', 'mode',
            'time_signature', 'track.album.id', 'track.album.release_date'))

# scale dataframe with continuous features
MP_continuous %<>% 
  scale() %>%
  data.frame()

# create dataframe with factors features
MP_factors <- songs_pop %>%
  select(c('key', 'mode', 'time_signature')) %>%
  data.frame()

# create cluster model (10-fold cv)
model <- kamila(MP_continuous, MP_factors, numClust = 2 : 10, numInit = 10, 
                 calcNumClust = "ps", numPredStrCvRun = 10, predStrThresh = 0.5)

# join clustering features
full_df <- songs_pop %>% left_join(clustering_features, by = 'id')

# label all songs in one of the existing one
full_df$cluster <- model$finalMemb
MP_continuous$cluster <- model$finalMemb
# MP_continuous$cluster <- model$finalMemb

# number of songs per cluster
table(full_df$cluster)

# best number of clusters (prediction strength method)
model$nClust$bestNClust

# undertanding the differences between clusters

# original scale
clusters_mean <- full_df %>%
  select(-c('id', 'track.album.release_date',
            'track.album.release_date_precision', 'track.album.id')) %>%
  group_by(cluster) %>%
  summarise(size = n(),
            danceability_mean = mean(danceability),
            energy_media = mean(energy),
            # key_median = median(key),
            loudness_mean = mean(loudness),
            # mode_median = meadian(mode),
            speechiness_mean = mean(speechiness),
            acousticness_mean = mean(acousticness),
            instrumentalness_mean = mean(instrumentalness),
            liveness_mean = mean(liveness),
            valence_mean = mean(valence),
            tempo_mean = mean(tempo),
            duration_ms_mean = mean(duration_ms),
            # time_signature_median = median(time_signature),
            popularity_mean = mean(track.popularity),
            days_since_release_median = median(days_since_release)
            ) %>%
  round(2)

# normalized scale
normalized_clusters_mean <- MP_continuous %>%
  group_by(cluster) %>%
  summarise(size = n(),
            danceability_mean = mean(danceability),
            energy_media = mean(energy),
            # key_median = median(key),
            loudness_mean = mean(loudness),
            # mode_median = meadian(mode),
            speechiness_mean = mean(speechiness),
            acousticness_mean = mean(acousticness),
            instrumentalness_mean = mean(instrumentalness),
            liveness_mean = mean(liveness),
            valence_mean = mean(valence),
            tempo_mean = mean(tempo),
            duration_ms_mean = mean(duration_ms),
            # time_signature_median = median(time_signature),
            # popularity_mean = mean(track.popularity),
            days_since_release_median = mean(days_since_release),
            days_until_pop_median = mean(days_until_pop),
            days_from_pop_to_top_median = mean(days_from_pop_to_top),
            days_from_top_until_leave_median = mean(days_from_top_until_leave)
  ) %>%
  round(2)

# create dataframe with continuous features removing features only present in pop songs
df_numerical <- all_songs

# create dataframe with continuous features
df_numerical %<>%
  select(-c('id', 'track.popularity','track.album.release_date',
            'track.album.release_date_precision', 'key', 'mode',
            'time_signature', 'track.album.id', 'track.album.release_date')) %>% 
  scale() %>%
  data.frame()

# create distances from clusters only with numerical features
dist_obj <- classDist(MP_continuous[, 1:10], as.factor(MP_continuous[, 15]))
dist_matrix <- predict(dist_obj, df_numerical)

all_songs[c('dist_c1', 'dist_c2', 'dist_c3', 'dist_c4')] <- dist_matrix

modeling_final <- all_songs %>% 
  select(-c('track.album.id', 'track.album.release_date', 'track.album.release_date_precision'))

modeling_final %<>% unique()

# x <- model$finalCenters
# x <- x[, 1:10]
# n <- model$nClust$bestNClust
# distances_df <- NULL
# temp <- NULL
# 
# for (row in 1:nrow(df_numerical)) {
#   # print(row)
#   temp <- NULL
#   
#   y <- df_numerical %>% slice(row) %>% as.list()
#   z <- dist(rbind(x, y)) %>% as.matrix()
#   z <- z['y', 1:n]
#   song_id <- all_songs %>%
#     select(c('id')) %>% 
#     slice(row)
# 
#   temp['id'] <- song_id
#   temp[c('dist_c1', 'dist_c2', 'dist_c3', 'dist_c4')] <- z
#   temp %<>% tbl_df()
#   
#   distances_df <- rbind(distances_df, temp)
#   # print(nrow(distances_df))
#   
# }

# save dataset to modeling
modeling_final %>%
  write_csv("2.Datasets/modeling_final.csv")
