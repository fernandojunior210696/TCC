# 1. Extracting data base for 200 most popular songs
# Last Generated: 2020-11-15

# Install required packages (needed once only)
# install.packages("spotifyr")
# install.packages("tidyverse")

# load required packages
library(tidyverse)
library(spotifyr)
library(magrittr)
library(lubridate)
library(foreach)
library(doParallel)
library(tcltk)


#setup parallel backend to use many processors
cores=detectCores()
cl <- makeCluster(cores-1)
registerDoParallel(cl)

# Client ID (get on spotify web api site for you)
Sys.setenv(SPOTIFY_CLIENT_ID = "3a341a4b68a243478346541d2efd4c9c")

# Client secret (get on spotify web api site for you)
Sys.setenv(SPOTIFY_CLIENT_SECRET = "0ba44167c53f4e5db45d7bef4302cd93")

# Generate authentication token
access_token <- get_spotify_access_token()

# get all categories ids avaibles in Brazil
# categories = c("toplists","brazilian","sertanejo","inspirational",
#                "mood","popculture","pop","chill","party","rock",
#                "hiphop","edm_dance","indie_alt","workout","kpop",
#                "romance","sleep","family","dinner","focus","soul",
#                "travel","classical","rnb","decades","reggae","jazz",
#                "arab","afro","j_tracks","test_latin","desi","sessions",
#                "blues","punk","metal","roots","comedy","gaming")

categories <- c('toplists','at_home','brazil','sertanejo','pop','blackhistorymonth',
'radar','samba___pagode','pride','wellness','rock','hiphop','inspirational',
'mpb','cities','edm_dance','indie_alt','latin','popculture','mood','party',
'chill','romance','sleep','workout','focus','decades','rnb','kpop',
'instrumental','jazz','classical','thirdparty','dinner','travel',
'caribbean','family','soul','arab','afro','j_tracks','desi','sessions',
'blues','punk','metal','roots','comedy','gaming')

# number of categories
length(categories)

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
musics <- foreach(i=less_100$id, .combine=rbind) %dopar% {
  
  tempMatrix = spotifyr::get_playlist_tracks(i, fields=target_names)
  if (ncol(tempMatrix)==6) {
    tempMatrix
  }
}

# loop to get all songs inside a playlist (only more than 100)
greater <- foreach(i=more_100$id, .combine=rbind) %dopar% {
  
  tempMatrix_100 = spotifyr::get_playlist_tracks(i, fields=target_names)
  tempMatrix_200 = spotifyr::get_playlist_tracks(i, fields=target_names, offset = 100)
  tempMatrix_300 = spotifyr::get_playlist_tracks(i, fields=target_names, offset = 200)
  tempMatrix_400 = spotifyr::get_playlist_tracks(i, fields=target_names, offset = 300)
  tempMatrix_500 = spotifyr::get_playlist_tracks(i, fields=target_names, offset = 400)
  tempMatrix_600 = spotifyr::get_playlist_tracks(i, fields=target_names, offset = 500)
  # print(ncol(tempMatrix_100))
  if (ncol(tempMatrix_100)==6) {
    do.call("rbind", list(tempMatrix_100, tempMatrix_200, tempMatrix_300, 
                          tempMatrix_400, tempMatrix_500, tempMatrix_600))
  }
}

# consolidate all songs
musics_id <- rbind(greater, musics)

# get unique artists ids
artists <- bind_rows(musics_id$track.artists)
artists %<>% unique()

# artists

# define progress bar for artists followers loop
pb = txtProgressBar(min = 0, max = length(seq(0,nrow(artists), 50)), initial = 0, style = 3) 

# get followers
artists_followers <- NULL
n=0
for (i in seq(0,nrow(artists), 50)) {
  n <- n+1
  setTxtProgressBar(pb,n)
  try({
    start=1+i
    end=50+i
    tempMatrix <- spotifyr::get_artists(artists$id[start:end])[c('id', 'followers.total')]
    artists_followers <- rbind(artists_followers, tempMatrix)
  })
  
}
close(pb)

# get followers for last artists
start=54150
end=nrow(artists)
tempMatrix <- spotifyr::get_artists(artists$id[start:end])[c('id', 'followers.total')]
artists_followers <- rbind(artists_followers, tempMatrix)

# unique artists followers
artists_followers %<>%
  unique()

# get from/to songs and artists
library(data.table)
musics_id %<>% unique()
artists_songs <- mapply(cbind, musics_id$track.artists, "track.id"=musics_id$track.id, SIMPLIFY = F)
artists_songs <- artists_songs[-12188]

artists_songs <- data.table::rbindlist(artists_songs, fill=TRUE)

# select only relevant columns
artists_songs %<>%
  select(c("track.id", "id"))

# get followers
artists_songs %<>% as_tibble()
artists_followers %<>% as_tibble()

artists_songs %<>% 
  left_join(artists_followers, by = c("id"="id"))

# songs info complete
musics_id %<>%
  as_tibble()

musics_id %<>% 
  left_join(artists_songs, by = c("track.id"="track.id"))

# remove unwanted information
musics_id %<>%
  select(-c("track.artists", "id"))

# unique songs
musics_id %<>% unique()

# average followers
musics_id %<>%
  group_by_at(setdiff(names(musics_id), "followers.total"))%>% 
  summarise(followers_mean = mean(followers.total))

# loop to get features for songs
# parallel
cores=detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)

# define progress bar for artists followers loop

pb = txtProgressBar(min = 0, max = length(seq(0,nrow(musics_id), 100)), initial = 0, style = 3) 

songs_features <- NULL
n=0
for (i in seq(0,nrow(musics_id), 100)) {
  n <- n+1
  setTxtProgressBar(pb,n)
  try({
    start=1+i
    end=100+i
    tempMatrix <- spotifyr::get_track_audio_features(musics_id$track.id[start:end])
    songs_features <- rbind(songs_features, cbind(musics_id$track.id[start:end], tempMatrix))
  })
  
}
close(pb)
stopCluster(cl)

# get features for last songs
start=94401
end=nrow(musics_id)
tempMatrix <- spotifyr::get_track_audio_features(musics_id$track.id[start:end])
songs_features <- rbind(songs_features, cbind(musics_id$track.id[start:end], tempMatrix))

songs_features %<>% 
  as_tibble()

songs_features %<>% rename(track.id = `musics_id$track.id[start:end]`)

# join all info
musics_features <- musics_id %>%
  left_join(songs_features, by = "track.id")

# delete unwanted features
musics_features %<>% 
  ungroup() %>%
  select(-c("track.album.album_type", "type", "id", "uri", "track_href", "analysis_url"))

musics_features$followers_mean %<>% as.integer()

# save database
write_csv(musics_features, "2.Datasets/songs_features_v2.csv") 
