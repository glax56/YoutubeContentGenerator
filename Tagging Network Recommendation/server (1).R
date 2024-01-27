#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#





# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  library(shiny)
  library(tidyverse)
  library(lubridate)
  library(magrittr)
  library(DBI)
  library(RSQLite)
  library(glue)
  library(igraph)
  library(tidygraph)
  library(scales)
  library(networkD3)

  
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
  random_video <- function(user_location) {
    
    dbGetQuery(conn, glue('select video_id from country_data where country = "{user_location}" order by random() limit 1'))
    
  }
  title_to_id <- function(title) {
    
    dbGetQuery(conn, glue('select video_id from country_data where title = "{title}" limit 1'))
    
  }
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
  
  t <- reactiveValues(data = NULL)
  k <- reactiveValues(data = NULL)
  
  observeEvent(input$start, {
    
  t$data <- recommendations(input$user_id, random_video(input$user_location), 
                            input$user_location, input$number)
  })
  
  observeEvent(input$recommend, {
    t$data <- recommendations(input$user_id, title_to_id(input$title), 
                              input$user_location, input$number)
  })
  
  observeEvent(input$reset, {
    dbExecute(conn, glue('drop table {input$user_id}_video_preferences'))
    dbExecute(conn, glue('drop table {input$user_id}_tags_preferences'))
  })
  
  observeEvent(input$keywordRecommend, {
    k$data <- like_videos(input$keywords, input$keyword_number, input$keyword_user_location)
  })
  
  output$recommendationsTable <- renderDataTable(t$data)
  
  output$recommendationsGraph <- renderForceNetwork({
    graph <- dbGetQuery(conn, glue('select * from {input$user_location}_video_edges')) %>%
      graph_from_data_frame(., directed = F) %>% 
      subgraph(., t$data$names) %>%
      set_vertex_attr('name', value = t$data$title) %>%
      igraph_to_networkD3( group = membership(cluster_fast_greedy(.))) 
    
    graph$nodes <- inner_join(graph$nodes, t$data %>% select(title, pr), by = join_by(name == title))
    
    forceNetwork(Links = graph$links, Nodes = graph$nodes, NodeID = 'name', 
                 Group = 'group', Nodesize = 'pr', height = 1000, width = 1000, zoom = T)
  })
  
  output$keywordRecommendationsTable <- renderDataTable(k$data)
  
  output$keywordRecommendationsGraph <- renderForceNetwork({
    graph <- dbGetQuery(conn, glue('select * from {input$user_location}_video_edges')) %>%
      graph_from_data_frame(., directed = F) %>% 
      subgraph(., k$data$names) %>%
      set_vertex_attr('name', value = k$data$title) %>%
      igraph_to_networkD3( group = membership(cluster_fast_greedy(.))) 
    
    graph$nodes <- inner_join(graph$nodes, k$data %>% select(title, pr), by = join_by(name == title))
    
    forceNetwork(Links = graph$links, Nodes = graph$nodes, NodeID = 'name', 
                 Group = 'group', Nodesize = 'pr', height = 1000, width = 1000, zoom = T)
  })

})
