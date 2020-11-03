# 1. Extracting data base for 200 most popular songs
# Last Generated: 2020-03-14

# Install required packages (needed once only)
# install.packages("spotifyr")
# install.packages("tidyverse")

# load required packages
library(tidyverse)
library(spotifyr)
library(magrittr)
library(lubridate)

# Client ID (get on spotify web api site for you)
Sys.setenv(SPOTIFY_CLIENT_ID = "3a341a4b68a243478346541d2efd4c9c")

# Client secret (get on spotify web api site for you)
Sys.setenv(SPOTIFY_CLIENT_SECRET = "0ba44167c53f4e5db45d7bef4302cd93")

# Generate authentication token
access_token <- get_spotify_access_token()

# get all categories ids avaibles in Brazil
categories = c("toplists","brazilian","sertanejo","inspirational",
               "mood","popculture","pop","chill","party","rock",
               "hiphop","edm_dance","indie_alt","workout","kpop",
               "romance","sleep","family","dinner","focus","soul",
               "travel","classical","rnb","decades","reggae","jazz",
               "arab","afro","j_tracks","test_latin","desi","sessions",
               "blues","punk","metal","roots","comedy","gaming")

# loop to get all playlists inside a category
playlists <- NULL

for (cat in categories) {
  
  # get first 50 playlists
  try({a <- get_category_playlists(cat,
                              country = "BR", 
                              limit = 50,
                              offset = 0)
  playlists <- rbind(playlists, a)
  
  # get first playlists from 51 to 100
  a <- get_category_playlists(cat,
                              country = "BR", 
                              limit = 50,
                              offset = 50)
  playlists <- rbind(playlists, a)
  
  # get first playlists from 101 to 150
  a <- get_category_playlists(cat,
                              country = "BR", 
                              limit = 50,
                              offset = 100)
  playlists <- rbind(playlists, a)})
  
}

# select unique playlist id, name and total numbers of songs
playlists %<>% 
  select(c("id", "name", "tracks.total")) %>%
  unique()

# separate into two playlist's categories:
# 1) less than 100 songs
less_100 <- playlists %>% filter(tracks.total <= 100)

# 2) more than 100 songs
more_100 <- playlists %>% filter(tracks.total > 100)

# loop to get all songs inside a playlist (only less than 100)
musics <- NULL

for (id in less_100$id) {
  
  try({a <- get_playlist_tracks(id,
                           fields = "items(track(id, popularity))")
  musics <- rbind(musics, a)})
  
}

# loop to get all songs inside a playlist (only more than 100)
greater <- NULL

for (id in more_100$id) {
  try({
  a <- get_playlist_tracks(id,
                           fields = "items(track(id, popularity))")
  greater <- rbind(greater, a)
  
  
  a <- get_playlist_tracks(id,
                           fields = "items(track(id, popularity))",
                           offset = 100)
  greater <- rbind(greater, a)
  
  
  a <- get_playlist_tracks(id,
                           fields = "items(track(id, popularity))",
                           offset = 200)
  greater <- rbind(greater, a)
  
  
  a <- get_playlist_tracks(id,
                           fields = "items(track(id, popularity))",
                           offset = 300)
  greater <- rbind(greater, a)
  
  
  a <- get_playlist_tracks(id,
                           fields = "items(track(id, popularity))",
                           offset = 400)
  greater <- rbind(greater, a)
  
  
  a <- get_playlist_tracks(id,
                           fields = "items(track(id, popularity))",
                           offset = 500)
  greater <- rbind(greater, a)
  
  
  a <- get_playlist_tracks(id,
                           fields = "items(track(id, popularity))",
                           offset = 600)
  greater <- rbind(greater, a)
  
  
  a <- get_playlist_tracks(id,
                           fields = "items(track(id, popularity))",
                           offset = 700)
  greater <- rbind(greater, a)
  
  
  a <- get_playlist_tracks(id,
                           fields = "items(track(id, popularity))",
                           offset = 800)
  greater <- rbind(greater, a)})
  
}

# consolidate all songs
musics_id <- rbind(greater, musics)

# get unique songs ids
musics_id %<>%
  unique()

# loop to get features for songs
musics_features <- NULL

for (id in musics_id$track.id) {
  a <- get_track_audio_features(id)
  
  try({musics_features <- rbind(musics_features, a)})
  
}

# get popularity information for all songs
musics_features %<>%
  left_join(musics_id, by = c("id" = "track.id"))

# select only important columns
musics_features %<>%
  select(c(13, 1:11, 17:19))

# loop to get features from album
album_df <- data.frame(id = character(),
                       track.album.id = character(),
                       track.album.release_date = character(),
                       track.album.release_date_precision = character())
done = 0
total = musics_features %>% nrow()
n_musics = 50

while (n_musics < total) {
  lista <- musics_features %>% slice((n_musics - 50) : n_musics) %>% select("id")
  a <- get_tracks(lista$id[1:50])
  n_musics = n_musics + 50
  try({album_df %<>% add_row(id = a$id,
                             track.album.id = a$album.id,
                             track.album.release_date = a$album.release_date,
                             track.album.release_date_precision = a$album.release_date_precision)})
  print(round(done/(total/50),4))
  done = done + 1
}

# join album information
musics_features %<>%
  left_join(album_df, by = "id")

# let's bring songs name
# loop to get features for songs
names <- NULL

for (id in all_songs$id) {
  a <- get_track(id)$name
  
  try({names <- rbind(names, a)})
  
}

a <- apply(matrix(all_songs$id, ncol=1), 1, get_track)


# save database
write_csv(musics_features, "features.csv") 
