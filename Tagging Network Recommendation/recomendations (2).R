# load required packages

library(tidyverse)
library(magrittr)
library(DBI)
library(RSQLite)
library(glue)
library(igraph)
library(scales)
library(networkD3)

#create database to store project

# step 1: download data from https://www.kaggle.com/datasets/rsrishav/youtube-trending-video-dataset and store in working driectory

# step 2: create sqlite database connection
conn <- dbConnect(RSQLite::SQLite(), 'recommendations.sqlite')

# step 3: read initial data in database under country_data

map_df(list.files()[str_detect(list.files(), '.csv')], ~read_csv(.x) %>%
         mutate(country = substring(.x, 1,2), .before = video_id)) %>%
  dbWriteTable(conn, 'country_data', .)

# step 4:  create vertical tags from country_data

dbGetQuery(conn, 'select * from country_data') %>%
  as_tibble() %>%
  select(country, video_id, tags) %>%
  separate_longer_delim(tags, delim = '|') %>%
  dbWriteTable(conn, 'vertical_tags', .)

# step 5:  create edges for videos, tags for each country

create_edges <- function(country_code) {
  
  dbGetQuery(conn, glue('select * from vertical_tags where country = "{country_code}"')) %>%
    filter(country == country_code) %>%
    inner_join(., ., by = 'tags', suffix = c('_one', '_two')) %>%
    filter(video_id_one > video_id_two) %>%
    distinct(video_id_one, video_id_two) %>%
    dbWriteTable(conn, glue('{country_code}_video_edges'), .)
  
  dbGetQuery(conn, glue('select * from vertical_tags where country = "{country_code}"')) %>%
    filter(country == country_code) %>%
    inner_join(., ., by = 'video_id', suffix = c('_one', '_two')) %>%
    filter(tags_one > tags_two) %>%
    distinct(tags_one, tags_two) %>%
    dbWriteTable(conn, glue('{country_code}_tags_edges'), .)
  
}

map(dbGetQuery(conn, 'select distinct country from country_data')$country, ~create_edges(.x))  

# calculate page rank with personalized vector

pr <- function(table, personalize) {

  dbGetQuery(conn, glue('select * from {table}')) %>%
    graph_from_data_frame(., directed = F) %>%
    page_rank(graph = ., directed = F, personalized = as_vector(personalize)) %$%
    tibble(names = names(vector), pr = vector)
}

# returns video recommendations based upon a users watch history

recommendations <- function(user_id, video_id, user_location, number) {
  
  # create watched, video_preferences, tags_preferences table if the user has no watch history
  
  dbExecute(conn, glue('create table if not exists {user_id}_watched (video_id text primary key)'))
  dbExecute(conn, glue('create table if not exists {user_id}_video_preferences (video_id text primary key, weight real, unique(video_id, weight))'))
  dbExecute(conn, glue('create table if not exists {user_id}_tags_preferences (tags primary key, weight real, unique(tags, weight))'))
  dbExecute(conn, glue('insert or ignore into {user_id}_video_preferences (video_id, weight) select distinct video_id, 0 as weight from country_data where country = "{user_location}"'))
  dbExecute(conn, glue('insert or ignore into {user_id}_tags_preferences (tags, weight) select distinct tags, 1 as weight from vertical_tags where country = "{user_location}"'))
  
  # insert newly watched video id in the user specific watched table, the tags of that video into the tags preferences video
  dbExecute(conn, glue('insert or ignore into {user_id}_watched (video_id) values("{video_id}")'))
  dbExecute(conn, glue('with watched_tags as (select distinct tags, 1 as weight from vertical_tags where video_id = "{video_id}")
  
                        update {user_id}_tags_preferences set weight = .9*weight + .1*(select weight from watched_tags where tags = {user_id}_tags_preferences.tags)  
                        where exists (select weight from watched_tags where tags = {user_id}_tags_preferences.tags)'))
  dbExecute(conn, glue('with watched_tags as (select distinct tags, 1 as weight from vertical_tags where video_id = "{video_id}")
                        
                        update {user_id}_tags_preferences set weight = .9*weight where tags not in (select tags from watched_tags)'))
  
  # calculate page rank values for tags in users locations with a personalized vector of tags from the videos they have watched
  
  tags_weight <- dbGetQuery(conn, glue('select * from {user_location}_tags_edges')) %>%
    graph_from_data_frame(., directed = F) %>%
    V(.) %>%
    bind_cols(country = user_location, vertices = ., names = names(.)) %>%
    left_join(., dbGetQuery(conn, glue('select * from {user_id}_tags_preferences')) %>% as_tibble, by = c('names'='tags')) %>%
    select(weight)
  
  pr(glue('{user_location}_tags_edges'), tags_weight) %>%
    dbWriteTable(conn, glue('{user_id}_tags_pr'), ., overwrite = T)
  
  # use these page rank values to calculate the personalized vector for the video page rank
  
  dbExecute(conn, glue('with watched_tags as (select distinct video_id, tags from vertical_tags where country = "{user_location}" and video_id = "{video_id}"),

          watched_count as (select video_id, count(tags) N from watched_tags),
          
          watched_join as ( select tags, N from watched_tags as t 
          inner join watched_count c on t.video_id = c.video_id),

           tags_ranks as (select distinct video_id, vertical.tags, pr, N from vertical_tags vertical inner join {user_id}_tags_pr pr
           on vertical.tags = pr.names inner join watched_join j
           on vertical.tags = j.tags
           and vertical.country = "{user_location}"),
           
           video_C as (select video_id, count(distinct tags) C from vertical_tags 
           where video_id in (select video_id from tags_ranks)
           and country = "{user_location}"
           group by video_id),
           
           raw_weights as (select ranks.video_id, count(tags) p, C, sum(pr) weight, N from tags_ranks ranks
           inner join video_C 
           on ranks.video_id = video_C.video_id
           group by ranks.video_id),
           
           new_weights as (select video_id, weight from raw_weights)
           
           update {user_id}_video_preferences set weight = .8*weight + .2*(select weight from new_weights where video_id = {user_id}_video_preferences.video_id)  
           where exists (select weight from new_weights where video_id = {user_id}_video_preferences.video_id)'))
  
  dbExecute(conn, glue('with watched_tags as (select distinct video_id, tags from vertical_tags where country = "{user_location}" and video_id = "{video_id}"),

          watched_count as (select video_id, count(tags) N from watched_tags),
          
          watched_join as ( select tags, N from watched_tags as t 
          inner join watched_count c on t.video_id = c.video_id),

           tags_ranks as (select distinct video_id, vertical.tags, pr, N from vertical_tags vertical inner join {user_id}_tags_pr pr
           on vertical.tags = pr.names inner join watched_join j
           on vertical.tags = j.tags
           and vertical.country = "{user_location}"),
           
            video_C as (select video_id, count(distinct tags) C from vertical_tags 
           where video_id in (select video_id from tags_ranks)
           and country = "{user_location}"
           group by video_id),
           
           raw_weights as (select video_id, count(tags) p , sum(pr) weight, N from tags_ranks
           group by video_id),
           
           new_weights as (select video_id, weight*(N-p)/ N as weight from raw_weights)
           
           update {user_id}_video_preferences set weight = .8*weight
                       where video_id not in (select video_id from new_weights)'))
  
  # calculate the page rank values for videos to make recommendations
  
  video_weight <- dbGetQuery(conn, glue('select * from {user_location}_video_edges where video_id_one not in 
                       (select * from {user_id}_watched) or video_id_two not in (select * from {user_id}_watched)')) %>%
    graph_from_data_frame(., directed = F) %>%
    V(.) %>%
    bind_cols(country = user_location, vertices = ., names = names(.)) %>%
    left_join(., dbGetQuery(conn, glue('select * from {user_id}_video_preferences')) %>% as_tibble, by = c('names'='video_id')) %>%
    select(weight)
  
  pr(glue('{user_location}_video_edges'), video_weight) %>%
    left_join(., dbGetQuery(conn, 'select distinct video_id, title, channel_title from country_data') %>% as_tibble(), by = c('names'='video_id')) %>%
    distinct(names, title, pr) %>%
    slice_max(order_by = pr, n = number) %>%
    mutate(pr = rescale(pr)*10)
  
  
  
}

# returns video recommendations based upon a set of keywords
     
like_videos <- function(keywords, number, location) {
  
  weights <- dbGetQuery(conn, glue('select distinct * from vertical_tags where country = "{location}"')) %>%
    as_tibble() %>%
    filter(tags %in% keywords) %>%
    inner_join(., dbGetQuery(conn, glue('select * from {location}_tags_edges')) %>%
                 graph_from_data_frame(., directed = F) %>%
                 page_rank(graph = .) %$%
                 tibble(tags = names(vector), pr = vector), by = 'tags') %>%
    mutate(video_id = as_factor(video_id)) %>%
    group_by(video_id) %>%
    summarise(video_weight = sum(pr))
  
  weight <- dbGetQuery(conn, glue('select * from {location}_video_edges')) %>%
    graph_from_data_frame() %>%
    V(.) %>%
    bind_cols(vertices = ., video_id = names(.)) %>%
    left_join(., weights, by = 'video_id') %>%
    mutate(video_weight = replace_na(video_weight, 0)) %>%
    select(video_weight)
  
  dbGetQuery(conn, glue('select * from {location}_video_edges')) %>%
    graph_from_data_frame() %>%
    page_rank(graph = ., personalized = as_vector(weight)) %$%
    tibble(video_id = names(vector), pr = vector) %>%
    left_join(., dbGetQuery(conn, 'select video_id, title, channel_title from country_data') %>% as_tibble(), by = 'video_id') %>%
    distinct(video_id, title, channel_title, pr) %>%
    slice_max(order_by = pr, n = number)
  
}

like_videos(c('food', 'comedy', 'italy'), 10, 'US')

# selects ranom video from country

random_video <- function(user_location) {
  
  dbGetQuery(conn, glue('select video_id from country_data where country = "{user_location}" order by random() limit 1'))
  
}

# converts title to video_id

title_to_id <- function(title) {
  
  dbGetQuery(conn, glue('select video_id from country_data where title = "{title}" limit 1'))
  
}

 
