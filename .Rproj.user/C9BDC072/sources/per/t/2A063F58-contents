# 3. Exploratory Data Analysis

# load required packages
library(tidyverse)
library(spotifyr)
library(magrittr)
library(lubridate)
library(corrplot)
library(zoo)
library(psych)

# read databases
all_songs <- read_csv('features.csv')
most_popular <- read_csv('most_popular.csv')

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

# select unique most popular songs
songs_pop <- most_popular %>%
  select(-c(date, mes, ano, ano_mes)) %>% 
  unique()

# select unique most popular songs monthly
monthly_pop <- most_popular %>%
  select(-c(date)) %>% 
  unique()

# flag each song as pop or not (since Jan 2017)
songs_pop$is_hit <- 1
all_songs %<>%
  left_join(songs_pop[, c(1, 19)], by = "id")

all_songs %<>% mutate(is_hit = coalesce(is_hit, 0))

# asjust the lables of the flags
all_songs$is_hit <- case_when(all_songs$is_hit == 1 ~ "Hit",
                         TRUE ~ "No Hit")

all_songs$is_hit %<>% as.factor()

# create monthly averages of features for most popular songs
ano_mes_pop <- monthly_pop %>% 
  group_by(ano_mes) %>% 
  summarise(popularity = mean(track.popularity),
            danceability = mean(danceability),
            energy = mean(energy),
            loudness = mean(loudness),
            speechiness = mean(speechiness),
            acousticness = mean(acousticness),
            instrumentalness = mean(instrumentalness),
            liveness = mean(liveness),
            valence = mean(valence),
            tempo = mean(tempo),
            duration_ms = mean(duration_ms))

# Questions to guide our EDA:
# 1. How is the popularity index calculated?
  # The value will be between 0 and 100, with 100 being the most popular.
  #The popularity is calculated by algorithm and is based, in the most part, 
  #on the total number of plays the track has had and how recent those plays are.

# Conclusion: songs most listened to recently will have higher popularity index

# 2. How is the popularity distributed 
all_songs %>%
  ggplot()+
  geom_histogram(aes(x = track.popularity, y = ..count..), 
                 fill = "#2980b9")+
  theme_minimal()+ 
  geom_vline(aes(xintercept = mean(all_songs$track.popularity)))+
  annotate(geom = "text", 
           x = mean(all_songs$track.popularity)+13, 
           y = 260, 
           label = paste0("Média: ", round(mean(all_songs$track.popularity), 3)))+
  labs(title = "Distribution of Song Popularity Index",
       subtitle = "All songs",
       x = "Popularity Index",
       y = "")

# Conclusion: it has a lot of songs with realy small popularity index and,
# for other songs, it follows a normal distribuition with a mean of 36.2

# 3. How is the popularity distributed for the songs that were on the 200 charts at any moment since Jan 2017?
songs_pop %>%
  ggplot()+
  geom_histogram(aes(x = track.popularity, y = ..count..), 
                 fill = "#2980b9")+
  theme_minimal()+ 
  geom_vline(aes(xintercept = mean(songs_pop$track.popularity)))+
  annotate(geom = "text", 
           x = mean(songs_pop$track.popularity)+13, 
           y = 260, 
           label = paste0("Média: ", round(mean(songs_pop$track.popularity), 3)))+
  labs(title = "Distribution of Song Popularity Index",
       subtitle = "Most Pop Songs (since Jan 2017)",
       x = "Popularity Index",
       y = "")

# Conclusion: it has a lot of songs with realy small popularity index and,
# for other songs, it follows a normal distribuition with a mean of 49.5

# 4. Can we compare, visually, the two distributions?
hit_x <- round(mean(all_songs$track.popularity[all_songs$is_hit == "Hit"]), 3)
no_hit_x <- round(mean(all_songs$track.popularity[all_songs$is_hit != "Hit"]), 3)

all_songs %>%
  ggplot()+
  geom_density(aes(x = track.popularity, 
                   fill = is_hit, 
                   alpha = 0.2,
                   color = is_hit))+
  theme_minimal()+ 
  geom_vline(aes(xintercept = hit_x,),
             color = "#267DB3")+
  annotate(geom = "text", 
           x = hit_x+10, 
           y = 0.03, 
           label = paste0("Média Hits: ", hit_x))+
  geom_vline(aes(xintercept = no_hit_x,),
             color = "#ED6647")+
  annotate(geom = "text", 
           x = no_hit_x+10, 
           y = 0.03, 
           label = paste0("Média No Hits: ", no_hit_x))+
  labs(title = "Distribution of Song Popularity Index",
       subtitle = "Hit Songs vs No Hit Songs",
       x = "Popularity Index",
       y = "")+
  scale_fill_manual(name = "is_hit", values = c("#267DB3", "#ED6647"))+
  scale_color_manual(name = "is_hit", values = c("#267DB3", "#ED6647"))

# 5. Is there any correlations between song features and popularity index?

# 5.1 For all songs
corrplot(cor(all_songs[, sapply(all_songs, is.numeric)]), method = "number")

# 5.2 For the most popular songs
corrplot(cor(most_popular[, sapply(most_popular, is.numeric)]), method = "number")

# Conclusion: There is no correlation between the features of the songs and popularity,
# which makes sense, since popularity index is based on changeable parameters

# 6. From 5, do the evolution of features for the most popular songs correlates with 
# popularity over time? It would make sense, if the tast of people is changing over time

# 6.1 How does popularity distribuition evolve for those songs?
monthly_pop %>%
  ggplot()+
  geom_boxplot(aes(x = ano_mes, y = track.popularity))+
  theme_minimal()+
  scale_x_discrete(breaks = levels(monthly_pop$ano_mes)[c(T, rep(F, 3))])

ano_mes_pop %>%
  ggplot()+
  geom_point(aes(x = ano_mes, y = popularity))+
  geom_smooth(aes(x = seq(1:nrow(ano_mes_pop)), 
                  y = popularity),
              method = lm)+
  labs(title = "Evolution of Mean Popularity of The Most Popular Songs Over Time",
       subtitle = "Linear Regression Tendency",
       x = "Ano Mes",
       y = "")+
  theme_minimal()+
  scale_x_discrete(breaks = levels(ano_mes_pop$ano_mes)[c(T, rep(F, 3))])

# 6.2 How is the correlation between the evolution of popularity index and the most 
# popular songs?
corrplot(cor(ano_mes_pop[, sapply(ano_mes_pop, is.numeric)]), method = "number")

# Conclusion: 

# From 6.1 we can say that the popular songs from the past don't, necesseraly
# have higher popularity today. Acctualy, what we observe is that it's lower than those
# from the songs that are more popular nowadays, which make sense by the way the index is
# calculated

# From 6.2 we can observe that the evolution of the features from the most popular song
# in each month is highly correlated with the popularity index. Threrefore, it can be used
# in order to create the model to infer the index.