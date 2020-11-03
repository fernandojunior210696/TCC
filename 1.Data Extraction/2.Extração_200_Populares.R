# 2. Extracting data base for 200 most popular songs
# Last Generated: 2020-03-14

# load required packages
library(tidyverse)
library(spotifyr)
library(magrittr)
library(lubridate)

# settings
setwd("~/Desktop/TCC")

# Client ID (get on spotify web api site for you)
Sys.setenv(SPOTIFY_CLIENT_ID = "3a341a4b68a243478346541d2efd4c9c")

# Client secret (get on spotify web api site for you)
Sys.setenv(SPOTIFY_CLIENT_SECRET = "0ba44167c53f4e5db45d7bef4302cd93")

# Generate authentication token
access_token <- get_spotify_access_token()

# Load database from script 1
musics_features <- read_csv('2.Datasets/features.csv')

# create range of date by one day since 01/01/2017
dates <- seq(ymd('2017-01-01'),ymd('2020-03-13'), 
             by = '1 day')

# change types
dates %<>% as.Date()
dates %<>% as.character()

# loop for accessing the site from sopify and download the songs
df <- NULL
data <- NULL
for (d in dates) {
  
  try({
    path <- paste0("https://spotifycharts.com/regional/br/daily/", paste0(d, "/download"))
    x <- read_csv(url(path), skip = 1)
    df <- rbind(df, x)
    y <- rep(d, 200)
    data <- rbind(data, data.frame(date = y))
  }) 
  
}

# combine songs with the date 
df <- cbind(data, df)

# create year and month from date
df$ano <- year(df$date)
df$mes <- month(df$date)

# extract song id
df$id <- gsub(".*\\/track/", "", df$URL)

backup <- df

# select only important columns
df %<>%
  select(c("date", "ano", "mes", "id", "Position"))

# get the features for the songs
df %<>% 
  left_join(musics_features, by = "id")

# identify songs without features 
no_match <- df %>% 
  filter(is.na(track.popularity)) %>%
  select(c("id")) %>%
  unique()

# loop to get popularity for songs without features 
no_match_popularity <- data.frame(id = character(),
                                  track.popularity = numeric(),
                                  track.album.id = character(),
                                  track.album.release_date = character(),
                                  track.album.release_date_precision = character())

for (id in no_match$id) {
  a <- get_track(id)
  try({no_match_popularity %<>% add_row(id = a$id, 
                                        track.popularity = a$popularity,
                                        track.album.id = a$album$id,
                                        track.album.release_date = a$album$release_date,
                                        track.album.release_date_precision = a$album$release_date_precision)})
  
}

# loop to get features for songs without features
no_match_features <- NULL

for (id in no_match$id) {
  a <- get_track_audio_features(id)
  
  try({no_match_features <- rbind(no_match_features, a)})
  
}

# consolidate no match information
no_match <- no_match_features %>%
  left_join(no_match_popularity, by = "id")

# add dates information
no_match %<>%
  left_join(df[1:5], by = "id")

# adjust columns order
no_match %<>% select(c("date", "ano", "mes", "Position",
                       "id", "danceability", "energy", 
                      "key", "loudness",  "mode", "speechiness", 
                      "acousticness", "instrumentalness", 
                      "liveness", "valence", "tempo", 
                      "duration_ms", "time_signature", 
                      "track.popularity", "track.album.id",
                      "track.album.release_date",
                      "track.album.release_date_precision"))
# filter songs with features
match <- df %>% 
  filter(!is.na(track.popularity))

# consolidate all songs
most_popular <- rbind(match, no_match)

# sort consolidated df
most_popular %<>% 
  arrange(date)

# save database
write_csv(most_popular, "2.Datasets/most_popular.csv") 
