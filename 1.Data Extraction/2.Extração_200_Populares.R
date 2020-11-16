# 2. Extracting data base for 200 most popular songs
# Last Generated: 2020-03-14

# load required packages
library(tidyverse)
library(spotifyr)
library(magrittr)
library(lubridate)
library(doParallel)
library(rvest)     
library(scales)
library(janitor)
library(furrr)
library(glue)
library(tidytext)

# settings
setwd("~/Desktop/TCC")

#setup parallel backend to use many processors

# Client ID (get on spotify web api site for you)
Sys.setenv(SPOTIFY_CLIENT_ID = "3a341a4b68a243478346541d2efd4c9c")

# Client secret (get on spotify web api site for you)
Sys.setenv(SPOTIFY_CLIENT_SECRET = "0ba44167c53f4e5db45d7bef4302cd93")

# Generate authentication token
access_token <- get_spotify_access_token()
# 
# Load database from script 1
musics_features <- read_csv('2.Datasets/songs_features_v2.csv')

datas_disponiveis <- read_html("https://www.spotifycharts.com/regional/br/daily/") %>%
  html_nodes(".responsive-select~ .responsive-select+ .responsive-select .responsive-select-value , .responsive-select li") %>%
  map(html_text) %>%
  map_chr(rbind)

# avaible dates
datas_disponiveis %<>%
  unique()

# selecting only dates
datas_disponivels_filtradas <- datas_disponiveis %>%
  enframe(name = NULL) %>%
  filter(str_detect(value, "\\d")) %>% 
  mutate(value = mdy(value)) %>%
  filter(year(value) %in% c(2017, 2018, 2019, 2020)) %>%
  distinct(value) %>%
  pull(value)

# get top songs since 2017

get_data <- function(x){
  read_csv(glue("https://spotifycharts.com/regional/br/daily/{x}/download"),
           skip = 1) %>%
    clean_names("upper_camel") %>%      # padronizando nomes das colunas
    mutate(data = x)
}

# Setup parallel
cores=detectCores()
cl <- makeCluster(cores-1)
registerDoParallel(cl)

# Get data
full_data <- future_map_dfr(datas_disponivels_filtradas, get_data, .progress = TRUE)

# Stop parallel
stopCluster(cl)

full_data$data %<>% as.Date()

# create year and month from date
full_data$ano <- full_data$data %>% as.Date() %>% year()
full_data$mes <- full_data$data %>% as.Date() %>% month()

# extract song id
full_data$id <- gsub(".*\\/track/", "", full_data$Url)

# select only important columns
full_data %<>%
  select(c("data", "ano", "mes", "id", "Position"))

# get the features for the songs
full_data %<>% 
  left_join(musics_features, by = c("id"="track.id"))

# identify songs without features 
no_match <- full_data %>% 
  filter(is.na(track.popularity)) %>%
  select(c("id")) %>%
  unique()

# filter songs with features
match <- full_data %>% 
  filter(!is.na(track.popularity))

# get features for unmatched songs
# define progress bar for artists followers loop
pb = txtProgressBar(min = 0, max = length(seq(0, nrow(no_match), by=100)), initial = 0, style = 3) 

# get followers
no_match_features <- NULL
n=0
for (i in seq(0, nrow(no_match), by=100)) {
  n <- n+1
  setTxtProgressBar(pb,n)
  try({
    start=1+i
    end=100+i
    tempMatrix <- spotifyr::get_track_audio_features(no_match$id[start:end])
    no_match_features <- rbind(no_match_features, tempMatrix)
  })
  
}
close(pb)

no_match_features %<>% drop_na()

# remove unwanted features
no_match_features %<>%
  select(-c("type", "uri", "track_href", "analysis_url")) %>% 
  unique()


# get name of songs with no match
# setup parallel 
cores=detectCores()
cl <- makeCluster(cores-1)
registerDoParallel(cl)

names_songs <- foreach(i=no_match_features$id, .combine=rbind) %dopar% {
  
  try({tempMatrix = spotifyr::get_tracks(i)})
  
  if (ncol(tempMatrix)==29) {
    tempMatrix
  }
    
}


names_songs %<>% 
  select(c("id", "name", "popularity", "album.release_date", "artists"))

# get artists songs
artists_songs <- mapply(cbind, names_songs$artists, "track.id"=names_songs$id, SIMPLIFY = F)
artists_songs <- data.table::rbindlist(artists_songs, fill=TRUE)

# get artists followers
artists_followers <- foreach(i=artists_songs$id, .combine=rbind) %dopar% {
  
  try({tempMatrix = spotifyr::get_artists(i)[c('id', 'followers.total')]})
  tempMatrix
}

# unique artists followers
artists_followers %<>%
  unique()

artists_songs %<>% 
  unique()

# get followers
artists_songs %<>% as_tibble()
artists_followers %<>% as_tibble()

artists_songs %<>% 
  left_join(artists_followers, by = c("id"="id"))

# remove unwanted information
artists_songs %<>%
  select(-c("id"))

# songs info complete
no_match_features %<>%
  as_tibble()

no_match_features <- names_songs %>% 
  left_join(no_match_features, by = c("id"="id"))

# remove unwanted information
no_match_features %<>%
  select(-c("artists"))

no_match_features %<>% 
  left_join(artists_songs[c("track.id", "followers.total")], by = c("id"="track.id"))

# average followers
no_match_features %<>%
  group_by_at(setdiff(names(no_match_features), c("followers.total")))%>% 
  summarise(followers_mean = mean(followers.total))

no_match_features %<>% ungroup()

# get dates info
no_match_complete <- full_data %>% 
  filter(is.na(track.popularity)) %>%
  select(c("data", "ano", "mes", "Position", "id")) %>%
  inner_join(no_match_features, by="id")

match_complete <- full_data %>% 
  filter(!is.na(track.popularity)) 

# rename columns
no_match_complete %<>%
  rename(c("track.name" = "name", "track.popularity" = "popularity", "track.album.release_date" = "album.release_date"))


# consolidate all songs
most_popular <- rbind(match_complete, no_match_complete)

# sort consolidated df
most_popular %<>% 
  arrange(data)

# rename columns
most_popular %<>%
  rename(c("track.id" = "id"))

# save database
write_csv(most_popular, "2.Datasets/most_popular_v2.csv") 

# full data set with songs features
musics_features <- most_popular %>% 
  select(-c("data", "ano", "mes", "Position")) %>% 
  unique() %>% 
  full_join(musics_features) %>% unique()

# save database
write_csv(musics_features, '2.Datasets/songs_features_v2.csv') 



