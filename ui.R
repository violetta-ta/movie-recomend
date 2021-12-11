## ui.R

# This application uses code provided at: https://github.com/pspachtholz/BookRecommender

library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

shinyUI(
    dashboardPage(
          skin = "yellow",
          dashboardHeader(title = "Movie Recommender"),
          
          dashboardSidebar(
            sidebarMenu(
              menuItem("Recommender by Genre", tabName = "recommenderByGenre", icon = icon("film")),
              menuItem("Recommender by Rating", tabName = "recommenderByRating", icon = icon("star"))
            )
          ),

          dashboardBody(includeCSS("css/movies.css"),
              tags$style(HTML("
                .skin-yellow .main-sidebar {
                  background-color:#292828;
                            }

                .box.box-solid.box-primary>.box-header {
                  color:#fff;
                  background:#292828;
                  height:50%
                }
                                    
                .box-header h3.box-title {
                  font-size: 24px;
                  padding: 5px;
                }
                
                .box.box-solid.box-primary{
                  border-bottom-color:#292828;
                  border-left-color:#292828;
                  border-right-color:#292828;
                  border-top-color:#292828;
                }
                          ")),
              tabItems(
                tabItem(tabName = "recommenderByGenre",
                    fluidRow(
                      box(width = 12, status = "primary", solidHeader = TRUE, collapsible = FALSE,
                          title = "Select your favorite genre: ",
                          column(width = 3,
                                 selectInput("genres", "Genres", 
                                             c("Action", "Adventure", "Animation", 
                                               "Children's", "Comedy", "Crime",
                                               "Documentary", "Drama", "Fantasy",
                                               "Film-Noir", "Horror", "Musical", 
                                               "Mystery", "Romance", "Sci-Fi", 
                                               "Thriller", "War", "Western"))
                          ),
                          column(width = 3,
                                 br(),
                                 withBusyIndicatorUI(
                                   actionButton("btn1", "Click here to get your recommendations", class = "btn-warning")
                                 )
                          )
                      ),
                      
                      box(width = 12, status = "primary", solidHeader = TRUE,
                          title = "Most popular movies of that genre: ",
                          br(),
                          tableOutput("results_system1")
                        
                      )
                    )
                ),
                
                tabItem(tabName = "recommenderByRating",
                    fluidRow(
                      box(width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                          title = "Step 1: Rate as many movies as possible", 
                          div(class = "rateitems",
                              uiOutput('ratings')
                          )
                      )
                    ),
                    
                    fluidRow(
                      useShinyjs(),
                      box(
                        width = 12, status = "primary", solidHeader = TRUE,
                        title = "Step 2: Discover movies you might like",
                        br(),
                        withBusyIndicatorUI(
                          actionButton("btn2", "Click here to get your recommendations", class = "btn-warning")
                        ),
                        br(),
                        tableOutput("results_system2")
                      )
                    )
                )
                
              )
          )
    )
) 