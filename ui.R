## ui.R
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
                  background:#292828
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
                      h3("Select your favorite genre: "),
                      column(width = 3, offset = 2, 
                             selectInput("genres", "Genres", 
                                         c("Action", "Adventure", "Animation", 
                                            "Children's", "Comedy", "Crime",
                                            "Documentary", "Drama", "Fantasy",
                                            "Film-Noir", "Horror", "Musical", 
                                            "Mystery", "Romance", "Sci-Fi", 
                                            "Thriller", "War", "Western"))
                      ),
                    ),
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
                          actionButton("btn", "Click here to get your recommendations", class = "default")
                        ),
                        br(),
                        tableOutput("results")
                      )
                    )
                )
                
              )
          )
    )
) 