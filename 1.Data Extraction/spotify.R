# Spotify TCC

# Instalar pacotes
# install.packages("spotifyr")
# install.packages("tidyverse")

# Carregar pacotes
library(tidyverse)
library(spotifyr)
library(magrittr)

# Set Up

setwd("~/R/Projetos/TCC")

# Client ID (Pegar do site do spotify web api)
Sys.setenv(SPOTIFY_CLIENT_ID = "3a341a4b68a243478346541d2efd4c9c")

# Client secret (Pegar do site do spotify web api)
Sys.setenv(SPOTIFY_CLIENT_SECRET = "0ba44167c53f4e5db45d7bef4302cd93")

# Gerar token de autorizaÃ§Ã£o
access_token <- get_spotify_access_token()

# Pegar de todas as categorias disponiveis no Brasil todas as playlists
# A partir daí, pegar todas as mÃºsicas disponíveis

# Todas categorias disponíveis no brasil em pt_br
categories = c("toplists","brazilian","sertanejo","inspirational",
               "mood","popculture","pop","chill","party","rock",
               "hiphop","edm_dance","indie_alt","workout","kpop",
               "romance","sleep","family","dinner","focus","soul",
               "travel","classical","rnb","decades","reggae","jazz",
               "arab","afro","j_tracks","test_latin","desi","sessions",
               "blues","punk","metal","roots","comedy","gaming")


# Criar variavel vazia
playlists <- NULL

# Loop for para trazer todas as playlists de cada categoria
for (cat in categories) {
  a <- get_category_playlists(cat,
                              country = "BR", 
                              limit = 50,
                              offset = 0)
  playlists <- rbind(playlists, a)
  
}

# Selecionando id e nomes de playlists unicos
playlists %<>% 
  select(c("id", "name", "tracks.total")) %>%
  unique()

# Separando 
less_100 <- playlists %>% filter(tracks.total <= 100)
more_100 <- playlists %>% filter(tracks.total > 100)

# Selecionando as músicas de cada playlist (menos que 100)
musics <- NULL

for (id in less_100$id) {
  a <- get_playlist_tracks(id,
                           fields = "items(track(id, popularity))")
  musics <- rbind(musics, a)
  
}

musics %<>%
  unique()

more_100 %<>% 
  filter(id != c("37i9dQZF1DWVY4eLfA3XFQ", 
                "5eJGpOCkZKa5pqENSWMXap"))

greater <- NULL

for (id in more_100$id) {
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
  greater <- rbind(greater, a)
  
}

greater %<>%
  unique()

musics_id <- rbind(greater, musics)

musics_id %<>%
  unique()

# Exportar
write_csv(musics_id, "id.csv")

musics_id %<>%
  filter(track.id != c("0Njk2f5tMOnr3CH2WAXpdU", 
                       "2dZXwYe7bHnLN7vHXIiGRB",
                       "05CKxyJP3ZFRyMkY9idpCZ",
                       "7EzkU8mQ3HLW4pfU8URb7N"))

# Selecionando as mÃºsicas de cada playlist (menos que 100)
musics_features <- NULL

for (id in musics_id$track.id[60360:62567]) {
  a <- get_track_audio_features(id)
  
    try({musics_features <- rbind(musics_features, a)})
  
}

# Salvar 
write_csv(musics_features, "features.csv") 

# Trazer a informação de popularidade
musics_features %<>%
  left_join(musics_id, by = c("id" = "track.id"))

# Selecionar variaveis importantes
musics_features %<>%
  select(c(13, 1:11, 17:19))

# Salvar 
write_csv(musics_features, "features.csv") 
