# 4. Clustering Kamila

# settings
setwd("~/Desktop/TCC")

# load required packages
library(tidyverse)
library(magrittr)
library(lubridate)
library(kamila)

# read databases
all_songs <- read_csv('2.Datasets/features.csv')
most_popular <- read_csv('2.Datasets/most_popular.csv')


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

## we are going to test some approachs in order to cluster the popular songs

# 1. Let's consider all songs that were popular at any moment 

# dataframe filtered
songs_pop <- most_popular %>%
  select(-c(date, mes, ano, ano_mes)) %>% 
  unique()

# create dataframe with continuous features
MP_continuous <- songs_pop %>%
  select(-c('id', 'track.popularity','track.album.release_date',
            'track.album.release_date_precision', 'key', 'mode',
            'time_signature', 'track.album.id')) # we could add some extra features (time since release)


# scale dataframe with continuous features
MP_continuous %<>% 
  scale() %>%
  data.frame()

# create dataframe with factors features
MP_factors <- songs_pop %>%
  select(c('key', 'mode', 'time_signature')) %>%
  data.frame()

# create cluster model
model <- kamila(MP_continuous, MP_factors, numClust = 2 : 10, numInit = 10, 
                 calcNumClust = "ps", numPredStrCvRun = 10, predStrThresh = 0.5)

# plot selected cluster
plot(2:10, model$nClust$psValues)

# best number of clusters (prediction strength method)
model$nClust$bestNClust

# classify new points to existing clusters
# classifyKamila(model, list(continuous_df, categorical_df))

# 2. Let's consider songs with minimun days at the top 200 each month

# 3. Let's create a segmentation each month

# From 1, we could calculate the distance from each song (all) with some of the clusters
# From 2, idem
# From 3, we could do the same, but for each month