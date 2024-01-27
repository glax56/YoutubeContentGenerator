#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("YouTube Recommendations"),
    
    tabsetPanel(
      tabPanel(title = 'watch history recommendations',
               sidebarLayout(
                 sidebarPanel(
                   textInput(inputId = 'user_id', label = 'User Id',
                             value = 'enter user Id'),
                   
                   selectInput(inputId = 'user_location', label = 'User Location', 
                               choices = dbGetQuery(conn, 'select distinct country from country_data') %>% tibble()),
                   
                   numericInput(inputId = 'number', label = 'Number', value = 0),
                   
                   textInput(inputId = 'title', label = 'Title'),
                   
                   actionButton(inputId = 'start', label = 'Initialize'), 
                   
                   actionButton(inputId = 'recommend', label = 'Recommend'),
                   
                   actionButton(inputId = 'reset', label = 'Reset')
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                   dataTableOutput("recommendationsTable"),
                   forceNetworkOutput("recommendationsGraph")
                 )
               )
               ),
      tabPanel(
        title = 'keywords recommendations',
        sidebarLayout(
          sidebarPanel(
            textInput(inputId = 'keywords', label = 'Keywords'),
            
            selectInput(inputId = 'keyword_user_location', label = 'User Location', 
                        choices = dbGetQuery(conn, 'select distinct country from country_data') %>% tibble()),
            
            numericInput(inputId = 'keyword_number', label = 'Number', value = 0),
            
            actionButton(inputId = 'keywordRecommend', label ='Recommend')
          ),
          mainPanel(
            dataTableOutput("keywordRecommendationsTable"),
            forceNetworkOutput('keywordRecommendationsGraph')
          )
        )
      )
    )

    # Sidebar with a slider input for number of bins
   
))
