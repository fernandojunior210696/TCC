# 3. Exploratory Data Analysis

# settings
setwd("~/Desktop/TCC")

# load required packages
library(tidyverse)
# library(spotifyr)
library(magrittr)
library(lubridate)
library(corrplot)
library(zoo)
library(psych)

# read databases
all_songs <- read_csv('2.Datasets/songs_features_v2.csv')
most_popular <- read_csv('2.Datasets/most_popular_v2.csv')

# remove songs with problem in songs features
all_songs <- all_songs[!is.na(all_songs$danceability),]
most_popular <- most_popular[!is.na(most_popular$danceability),]

# encode factors features
all_songs$time_signature %<>% as.factor()
all_songs$mode %<>% as.factor()
all_songs$key %<>% as.factor()

most_popular$time_signature %<>% as.factor()
most_popular$mode %<>% as.factor()
most_popular$key %<>% as.factor()

# encode date features
all_songs$track.album.release_date %<>% as.Date()
most_popular$track.album.release_date %<>% as.Date()

# create age of songs
all_songs$date_ref <- ymd('2020-11-15')
most_popular$date_ref <- ymd('2020-11-15')

# age of songs
all_songs$days_since_release <- all_songs$date_ref-all_songs$track.album.release_date
most_popular$days_since_release <- most_popular$date_ref-most_popular$track.album.release_date

all_songs$days_since_release %<>% as.integer()
most_popular$days_since_release %<>% as.integer()

# rename most_popular date colum
most_popular %<>% rename(c("date" = "data"))
most_popular$date %<>% as.Date()

most_popular$ano_mes <- as.yearmon(most_popular$date)
most_popular$ano_mes %<>% factor(ordered = TRUE)

# select unique most popular songs
songs_pop <- most_popular %>%
  select(-c(date, mes, ano, ano_mes, Position)) %>% 
  unique()

# select unique most popular songs monthly
monthly_pop <- most_popular %>%
  select(-c(date, Position)) %>% 
  unique()

# flag each song as pop or not based on today index
pop_today <- most_popular %>%
  filter(date == '2020-11-15') 

pop_today$is_hit <- 1
all_songs %<>%
  left_join(pop_today[, c("track.id", "is_hit")], by = "track.id")

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

# Days of popularity
qnt_days_pop <- most_popular %>%
  group_by(track.id) %>%
  summarise(days_of_populatiry = n()) 

most_popular %<>%
  left_join(qnt_days_pop)

# Days until pop
min_pop_date <- most_popular %>%
  group_by(track.id, track.album.release_date) %>%
  summarise(pop_date = min(date))

min_pop_date$days_until_pop <- min_pop_date$pop_date - min_pop_date$track.album.release_date

most_popular %<>%
  left_join(min_pop_date[c("track.id", "days_until_pop", "pop_date")])

# Days from pop to top
top_position <- most_popular %>%
  select(c('date', 'track.id', 'Position')) %>%
  group_by(track.id) %>%
  summarise(position_max = min(Position))

top_day <- most_popular %>%
  select(c('date', 'track.id', 'Position')) %>%
  left_join(top_position, by=c('track.id')) %>%
  filter(Position==position_max) %>%
  group_by(track.id, position_max) %>%
  summarise(position_max_date = min(date))

most_popular %<>%
  left_join(top_day[c("track.id", "position_max_date")])

most_popular$days_from_pop_to_top <- most_popular$position_max_date-most_popular$pop_date

# Days from top until leave
max_pop_date <- most_popular %>%
  group_by(track.id) %>%
  summarise(max_pop_date = max(date))

most_popular %<>%
  left_join(max_pop_date, by='track.id')

most_popular$days_from_top_until_leave <-  most_popular$max_pop_date-most_popular$position_max_date

# Questions to guide our EDA:
# 1. How is the popularity index calculated?
  # The value will be between 0 and 100, with 100 being the most popular.
  #The popularity is calculated by algorithm and is based, in the most part, 
  #on the total number of plays the track has had and how recent those plays are.

# Conclusion: songs most listened to recently will have higher popularity index

# 2. How is the popularity distributed 
all_songs %>%
  ggplot()+
  geom_density(aes(x = track.popularity, y = ..count..), 
                 fill = "#2980b9")+
  theme_minimal()+ 
  geom_vline(aes(xintercept = mean(all_songs$track.popularity)))+
  annotate(geom = "text", 
           x = mean(all_songs$track.popularity)+13, 
           y = 2400, 
           label = paste0("Média: ", round(mean(all_songs$track.popularity), 2)))+
  labs(title = "Distribuição do índice de popularidade",
       subtitle = "Todas as músicas",
       x = "Índice de Popularidade",
       y = "")+ 
  theme_classic()

# Conclusion: it has a lot of songs with realy small popularity index and,
# for other songs, it follows a normal distribuition with a mean of 36.2

# 2.1 Why are there songs with realy small values?
# ???

# 2.2 Is the distribuition close to normality?
qqnorm(all_songs$track.popularity, pch = 1, frame = FALSE, 
       main = "Q-Q Plot para normalidade",
       xlab = "Quantis teóricos",
       ylab = "Quantis da amostra")
qqline(all_songs$track.popularity, 
       col = "steelblue", 
       lwd = 2, 
       main = "Q-Q Plot para normalidade",
       xlab = "Quantis teóricos",
       ylab = "Quantis da amostra")

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
       y = "")+ 
  theme_classic()

# 3.1 How is the popularity distributed for the songs that were at the 200 charts in the last day of records since Jan 2017?
pop_today %>%
  ggplot()+
  geom_density(aes(x = track.popularity, y = ..count..), 
                 fill = "#2980b9")+
  theme_minimal()+ 
  geom_vline(aes(xintercept = mean(pop_today$track.popularity)))+
  annotate(geom = "text", 
           x = mean(pop_today$track.popularity)+13, 
           y = 15, 
           label = paste0("Média: ", round(mean(pop_today$track.popularity), 3)))+
  labs(title = "Distribution of Song Popularity Index",
       subtitle = "Most Pop Songs (since Jan 2017)",
       x = "Popularity Index",
       y = "")+ 
  theme_classic()

# From 3.1, we can observe that the mean value of popular index is 74, meaning that, 
# these songs at the 200 chart have higher index value.
# However, there are songs with realy low index

# 4. Can we compare, visually, the two distributions?
hit_x <- round(mean(all_songs$track.popularity[all_songs$is_hit == "Hit"]), 2)
no_hit_x <- round(mean(all_songs$track.popularity[all_songs$is_hit != "Hit"]), 2)

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
           x = hit_x+15, 
           y = 0.07, 
           label = paste0("Média: ", hit_x),
           color = "#267DB3")+
  geom_vline(aes(xintercept = no_hit_x,),
             color = "#ED6647")+
  annotate(geom = "text", 
           x = no_hit_x+15, 
           y = 0.07, 
           label = paste0("Média: ", no_hit_x),
           color = "#ED6647")+
  labs(title = "Distribuição do índice de popularidade",
       subtitle = "Músicas populares vs demais",
       x = "Índice de Popularidade",
       y = "")+
  scale_fill_manual(name = "is_hit", values = c("#267DB3", "#ED6647"))+
  scale_color_manual(name = "is_hit", values = c("#267DB3", "#ED6647"))+ 
  theme_classic()

# 5. Is there any correlations between song features and popularity index?


# 5.1 For all songs
corrplot(cor(all_songs[complete.cases(all_songs), sapply(all_songs, is.numeric)]), method = "number")

# 5.2 For the most popular songs
corrplot(cor(most_popular[complete.cases(most_popular), sapply(most_popular, is.numeric)]), method = "number")

# Conclusion: There is no correlation between the features of the songs and popularity,
# which makes sense, since popularity index is based on changeable parameters

# 6. From 5, do the evolution of features for the most popular songs correlates with 
# popularity over time? It would make sense, if the taste of people is changing over time

# 6.1 How does popularity distribution evolve for those songs?
monthly_pop %>%
  ggplot()+
  geom_boxplot(aes(x = ano_mes, y = track.popularity))+
  theme_minimal()+
  scale_x_discrete(breaks = levels(monthly_pop$ano_mes)[c(T, rep(F, 3))])+
  labs(title = "Distribuição mensal do índice de popularidade",
       subtitle = "Músicas populares a cada mês",
       x = "Mês-Ano popular",
       y = "Índice de popularidade")+
  theme_minimal()

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

# # 6.2 How is the correlation between the evolution of popularity index and the most 
# # popular songs?
# corrplot(cor(ano_mes_pop[, sapply(ano_mes_pop, is.numeric)]), method = "number")

# Conclusion: 

# From 6.1 we can say that the popular songs from the past don't, necesseraly
# have higher popularity today. Acctualy, what we observe is that it's lower than those
# from the songs that are more popular nowadays, which make sense by the way the index is
# calculated

# 7 How many popular (Top 200) songs are there in each month?
qnt_monthly_pop <- monthly_pop %>%
  group_by(ano_mes) %>%
  summarise(count = n()) 
  
qnt_monthly_pop %>%
  ggplot()+
  geom_density(aes(x = count, y = ..count..), 
           fill = "#2980b9")+
  theme_minimal()+
  geom_vline(aes(xintercept = mean(qnt_monthly_pop$count)))+
  annotate(geom = "text",
           x = mean(qnt_monthly_pop$count)+13,
           y = 1,
           label = paste0("Média: ", round(mean(qnt_monthly_pop$count), 3)))+
  labs(title = "Distribution of Songs in Each Month",
       subtitle = "Most Pop Songs (since Jan 2017)",
       x = "Count",
       y = "")+
  theme_minimal()

# 8. How many days a song remain at the top 200?
qnt_days_pop <- most_popular %>%
  group_by(track.id) %>%
  summarise(days_of_populatiry = n()) 

qnt_days_pop %>%
  ggplot()+
  geom_density(aes(x = days_of_populatiry, y = ..count..), 
               fill = "#2980b9")+
  theme_minimal()+
  geom_vline(aes(xintercept = mean(days_of_populatiry)))+
  annotate(geom = "text",
           x = mean(qnt_days_pop$days_of_populatiry)+100,
           y = 49,
           label = paste0("Média: ", round(mean(qnt_days_pop$days_of_populatiry), 3)))+
  geom_vline(aes(xintercept = median(days_of_populatiry)))+
  annotate(geom = "text",
           x = median(qnt_days_pop$days_of_populatiry)+100,
           y = 20,
           label = paste0("Mediano: ", round(median(qnt_days_pop$days_of_populatiry), 3)))+
  labs(title = "Distribuição de dias que uma música permanece nas Top 200",
       subtitle = "Músicas mais tocadas desde 2017",
       x = "Dias",
       y = "Densidade")+
  theme_minimal()

# From 8, there are some outliers and the distribuition is assimetrical

# 8.1 Analysing distribuition in order to remove outliers
qnt_days_pop$days_of_populatiry %>% 
  quantile(probs = c(seq(0, 0.75, 0.25), seq(0.9, 1, 0.01)))

# From 8.1, we can remove observation above 97% (387)
qnt_days_pop %<>%
  filter(days_of_populatiry < 406) 

# 8.2 Analysing the distribuition without outliers
qnt_days_pop %>%
  ggplot()+
  geom_density(aes(x = days_of_populatiry, y = ..count..), 
               fill = "#2980b9")+
  theme_minimal()+
  geom_vline(aes(xintercept = mean(qnt_days_pop$days_of_populatiry)),
             color = '#8e44ad')+
  geom_vline(aes(xintercept = median(qnt_days_pop$days_of_populatiry)),
             color = '#27ae60')+
  annotate(geom = "text",
           x = mean(qnt_days_pop$days_of_populatiry)+23,
           y = 40,
           label = paste0("Média: ", round(mean(qnt_days_pop$days_of_populatiry), 2)),
           color = '#8e44ad')+
  annotate(geom = "text",
           x = median(qnt_days_pop$days_of_populatiry)+23,
           y = 50,
           label = paste0("Mediana: ", round(median(qnt_days_pop$days_of_populatiry), 3)),
           color = '#27ae60')+
  labs(title = "Distribuição de dias que uma música permanece nas Top 200",
       subtitle = "Músicas mais tocadas desde 2017",
       x = "Dias",
       y = "Densidade")+
  theme_minimal()

# From 8.2, the distribuition is very assimetrical. There are lots of songs with few
# days in top 200 and a few songs with a lot of days in the top 200 ranking

# 9. How many songs remain at the top 200 in each month?
most_popular$days_in_month <- days_in_month(most_popular$mes)

songs_remaining <- most_popular %>%
  group_by(track.id, ano_mes, days_in_month) %>%
  summarise(days = n()) %>%
  filter(days_in_month == days)

monthly_remaining <- songs_remaining %>%
  group_by(ano_mes) %>%
  summarise(songs = n())

monthly_remaining %>%
  select(songs) %>%
  summary()

# From 9, we can see that there are, on average, 132 songs remaning popular in each month,
# so we could define our monthly songs to be clustered as the songs that remain on top.

# From 7, we can see that there are, on average, 307 different songs at the top 200 ranking
# so, as another aproach, we could use all songs that were popular in each month to be clustered

# 10. When were songs released, considering both the populars and all other songs

# 10.1 all songs

all_songs %>% 
  ggplot()+
  geom_histogram(aes(x = track.album.release_date, y= ..count..), 
                 fill = '#2980b9')+
  theme_minimal()+ 
  geom_vline(aes(xintercept = median(all_songs$track.album.release_date, na.rm = T)))+
  annotate(geom = "text", 
           x = median(all_songs$track.album.release_date, na.rm = T)-5000, 
           y = 20000, 
           label = paste0("Mediana: ", year(median(all_songs$track.album.release_date, na.rm = T))))+
  labs(title = "Distribution of Song Release Date",
       subtitle = "All songs",
       x = "Release Date",
       y = "")

all_songs %>% 
  select(track.album.release_date) %>% 
  summary()

# 10.2 popular songs

songs_pop %>% 
  ggplot()+
  geom_histogram(aes(x = track.album.release_date, y= ..count..), 
                 fill = '#2980b9')+
  theme_minimal()+ 
  geom_vline(aes(xintercept = median(songs_pop$track.album.release_date, na.rm = T)))+
  annotate(geom = "text", 
           x = median(songs_pop$track.album.release_date, na.rm = T)-5000, 
           y = 4000, 
           label = paste0("Mediana: ", year(median(songs_pop$track.album.release_date, na.rm = T))))+
  labs(title = "Distribution of Song Release Date",
       subtitle = "Popular songs (200 Top)",
       x = "Release Date",
       y = "")

songs_pop %>% 
  select(track.album.release_date) %>% 
  summary()


# 11. How many days a song takes to become popular
min_pop_date <- most_popular %>%
  group_by(track.id, track.album.release_date) %>%
  summarise(pop_date = min(date))

min_pop_date$days_until_pop <- min_pop_date$pop_date - min_pop_date$track.album.release_date

min_pop_date %>% 
  ggplot()+
  geom_density(aes(x = days_until_pop), 
               fill = '#2980b9')+
  theme_minimal()+
  geom_vline(aes(xintercept = median(min_pop_date$days_until_pop, na.rm = T)))+
  annotate(geom = "text", 
           x = median(min_pop_date$days_until_pop, na.rm = T)+2000, 
           y = 0.02, 
           label = paste0("Mediana: ", median(min_pop_date$days_until_pop, na.rm = T)))+
  labs(title = "Distribution of Song Release Date",
       subtitle = "Popular songs (200 Top)",
       x = "Release Date",
       y = "")

# verifying outliers
min_pop_date$days_until_pop %>% quantile(na.rm = TRUE, probs = c(0, 0.5, 0.9, 0.95, 0.99))

min_pop_date %<>% 
  filter(days_until_pop > 0 & days_until_pop <= 1443) 
min_pop_date %>%
  ggplot()+
  geom_density(aes(x = days_until_pop), 
               fill = '#2980b9')+
  theme_minimal()+
  geom_vline(aes(xintercept = mean(min_pop_date$days_until_pop, na.rm = T)),
             color = '#8e44ad')+
  annotate(geom = "text", 
           x = mean(min_pop_date$days_until_pop, na.rm = T)+100, 
           y = 0.015, 
           label = paste0("Média: ", round(mean(min_pop_date$days_until_pop, na.rm = T), 2)),
           color = '#8e44ad'
           )+
  geom_vline(aes(xintercept = median(min_pop_date$days_until_pop, na.rm = T)),
             color = '#27ae60')+
  annotate(geom = "text", 
           x = median(min_pop_date$days_until_pop, na.rm = T)+100, 
           y = 0.010, 
           label = paste0("Mediana: ", round(median(min_pop_date$days_until_pop, na.rm = T), 3)),
           color = '#27ae60')+
  labs(title = "Distribuição de dias até entrar no top 200",
       subtitle = "Músicas que foram populares (top 200)",
       x = "Dias até a popularidade",
       y = "")

# 12. How many days a song takes to go to the top position
top_position <- most_popular %>%
  select(c('date', 'track.id', 'Position')) %>%
  group_by(track.id) %>%
  summarise(position_max = min(Position))

top_day <- most_popular %>%
  select(c('date', 'track.id', 'Position')) %>%
  left_join(top_position, by=c('track.id')) %>%
  filter(Position==position_max) %>%
  group_by(track.id, position_max) %>%
  summarise(position_max_date = min(date))


# plot distribuition
days_from_pop_to_top_plot <- most_popular %>% 
  filter(days_from_pop_to_top > 0 & days_from_pop_to_top < 1000) 

days_from_pop_to_top_plot %>%
  ggplot()+
  geom_density(aes(x = days_from_pop_to_top), 
               fill = '#2980b9')+
  theme_minimal()+
  geom_vline(aes(xintercept = mean(days_from_pop_to_top_plot$days_from_pop_to_top, na.rm = T)),
             color = '#8e44ad')+
  annotate(geom = "text", 
           x = mean(days_from_pop_to_top_plot$days_from_pop_to_top, na.rm = T)+70, 
           y = 0.015, 
           label = paste0("Média: ", round(mean(days_from_pop_to_top_plot$days_from_pop_to_top, na.rm = T), 2)),
           color = '#8e44ad'
  )+
  geom_vline(aes(xintercept = median(days_from_pop_to_top_plot$days_from_pop_to_top, na.rm = T)),
             color = '#27ae60')+
  annotate(geom = "text", 
           x = median(days_from_pop_to_top_plot$days_from_pop_to_top, na.rm = T)+70, 
           y = 0.010, 
           label = paste0("Mediana: ", round(median(days_from_pop_to_top_plot$days_from_pop_to_top, na.rm = T), 3)),
           color = '#27ae60')+
  labs(title = "Distribuição de dias até o primeiro topo",
       subtitle = "Músicas que foram populares (top 200)",
       x = "Dias até o topo",
       y = "")
  

# 13. How many days a song takes to leave the 200 top chart?

max_pop_date <- most_popular %>%
  group_by(track.id) %>%
  summarise(max_pop_date = max(date))

# plot distribuition
days_top_leave_plot <- most_popular %>% 
  filter(days_from_top_until_leave > 0 & days_from_top_until_leave < 1000) 

days_top_leave_plot %>%
  ggplot()+
  geom_density(aes(x = days_from_top_until_leave), 
               fill = '#2980b9')+
  theme_minimal()+
  geom_vline(aes(xintercept = mean(days_top_leave_plot$days_from_top_until_leave, na.rm = T)),
           color = '#8e44ad')+
  annotate(geom = "text", 
           x = mean(days_top_leave_plot$days_from_top_until_leave, na.rm = T)+70, 
           y = 0.004, 
           label = paste0("Média: ", round(mean(days_top_leave_plot$days_from_top_until_leave, na.rm = T), 2)),
           color = '#8e44ad'
  )+
  geom_vline(aes(xintercept = median(days_top_leave_plot$days_from_top_until_leave, na.rm = T)),
             color = '#27ae60')+
  annotate(geom = "text", 
           x = median(days_top_leave_plot$days_from_top_until_leave, na.rm = T)+70, 
           y = 0.003, 
           label = paste0("Mediana: ", round(median(days_top_leave_plot$days_from_top_until_leave, na.rm = T), 3)),
           color = '#27ae60')+
  labs(title = "Distribuição de dias do topo até sair do ranking",
       subtitle = "Músicas que foram populares (top 200)",
       x = "Quantidade de dias",
       y = "")

# 14. How are age of songs distribuited

most_popular$days_since_release %<>% as.integer()

# all songs plot age
days_since_release_plot <- all_songs %>% 
  filter(days_since_release > 0 & days_since_release < 1000) 

days_since_release_plot %>%
  ggplot()+
  geom_density(aes(x = days_since_release), 
               fill = '#2980b9')+
  theme_minimal()+
  geom_vline(aes(xintercept = mean(days_since_release_plot$days_since_release, na.rm = T)))+
  annotate(geom = "text", 
           x = mean(days_since_release_plot$days_since_release, na.rm = T)+100, 
           y = 0.003, 
           label = paste0("Média: ", mean(days_since_release_plot$days_since_release, na.rm = T)))+
  labs(title = "Distribution of Days Since Release",
       subtitle = "All songs",
       x = "Days Since Release",
       y = "")

# top songs plot age
days_since_release_plot <- most_popular %>% 
  filter(days_since_release > 0 & days_since_release < 1000) 

days_since_release_plot %>%
  ggplot()+
  geom_density(aes(x = days_since_release), 
               fill = '#2980b9')+
  theme_minimal()+
  geom_vline(aes(xintercept = mean(days_since_release_plot$days_since_release, na.rm = T)))+
  annotate(geom = "text", 
           x = mean(days_since_release_plot$days_since_release, na.rm = T)+100, 
           y = 0.003, 
           label = paste0("Média: ", mean(days_since_release_plot$days_since_release, na.rm = T)))+
  labs(title = "Distribution of Days Since Release",
       subtitle = "Popular songs (200 Top)",
       x = "Days Since Release",
       y = "")

# save dataset to clustering
most_popular %>%
  select(c(track.id, days_since_release, days_until_pop, days_from_pop_to_top, days_from_top_until_leave, days_of_populatiry)) %>%
  unique() %>%
  write_csv("2.Datasets/clusterig_features_v2.csv")

