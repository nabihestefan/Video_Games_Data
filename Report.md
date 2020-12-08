---
title: "Report"
author: "Julian & Nabih"
date: "12/8/2020"
output: html_document
---
## Setup
```{r setup, echo=FALSE}
library(tidyverse)
```
## Load Data 
```{r load-data, message=FALSE}
game_genres <- "./data/game_genres.csv"
df_genre <- read_csv(game_genres) %>% rename(genre = Genre)

game_ids <- "./data/game_ids.csv"
df_ids <- read_csv(game_ids)

player_data <- "./data/player_data.csv"
df_player <- 
  read_csv(player_data) %>% 
  filter(action == "play") %>% 
  select(user_id, name, hours)
```
## First look into our thre datasets
```{r summary}
summary(df_genre)
summary(df_ids)
summary(df_player)
```
## Join and clean data
```{r join-data}
df_game <-
  df_genre %>% 
  left_join(df_ids, by = "appid") %>% 
  select(name, genre) %>% 
  drop_na() %>% 
  unique() %>% 
  mutate(
    name = str_remove(name, ":"),
    name = str_replace(name, "’", "'")
  )

df_data <-
  df_player %>% 
    left_join(df_game, by = "name")

df_data[is.na(df_data)] <- "Unidentified"

summary(df_data)
```