## server.R

# load functions
source('functions/cf_algorithm.R') # collaborative filtering
source('functions/similarity_measures.R') # similarity measures
source('functions/genre_algorithm.R') #genre algo, system 1
source('functions/helpers.R')

# define functions
get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

# read in data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

movies$seqno = 1:nrow(movies)

# Splitting the genres into different Movie genre columns
movie_genre_df = as.data.frame(movies$Genres,stringsAsFactors = FALSE)
movie_genre_df = as.data.frame(tstrsplit(movie_genre_df[,1],"[|]",type.convert = TRUE))
colnames(movie_genre_df) = c("Genre1","Genre2","Genre3","Genre4","Genre5","Genre6")
movie_genre_df[,"MovieID"] = movies$MovieID

# reading the ratings data
ratings = read.csv(file = './data/ratings/ratings.dat', 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

#Creating Rating Matrix
i = paste0('u', ratings$UserID)
j = paste0('m', ratings$MovieID)
x = ratings$Rating
tmp = data.frame(i, j, x, stringsAsFactors = T)
Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(Rmat) = levels(tmp$i)
colnames(Rmat) = levels(tmp$j)
Rmat = new('realRatingMatrix', data = Rmat)

#Creating the test/training split
schema_1 = evaluationScheme(Rmat, method="split", train=0.8, given=15, k=1)

#Training the recommender. This and the previous steps need to be performed at the runtime of the app, as they all take around 3 minutes.
rec_UBCF1 = Recommender(getData(schema_1, "train"), method = 'UBCF',
                        parameter = list(normalize = 'Z-score', 
                                         method = 'Cosine',
                                         weighted = TRUE,
                                         nn = 25))


shinyServer(function(input, output, session) {
  # =============
  #System 1 
  # =============
  
  
  df_system1 <- eventReactive(input$btn1, {
    withBusyIndicatorServer("btn1", { # showing the busy indicator
      # hide the rating container
      #useShinyjs()
      #jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      #runjs(jsCode)
      
      #get input
      genreSelected = input$genres
      
      user_results = (1:10)/10
      user_predicted_ids = get_movie_genre_recomm1(genreSelected, movie_genre_df, ratings)
      recom_results <- data.table(Rank = 1:10, 
                                  MovieID = movies$MovieID[user_predicted_ids], 
                                  Title = movies$Title[user_predicted_ids], 
                                  Predicted_rating =  user_results)
     
   }) # still busy
   
  })
  
  
  output$results_system1 <- renderUI({
   num_rows <- 2
   num_movies <- 5
   recom_result <- df_system1()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
           div(style = "text-align:center", 
                a(img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
           ),
          div(style="text-align:center; font-size: 100%", 
              strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
          )
            
       )        
     }))) # columns
   }) # rows
    
   })# renderUI
  
  
  # =============
  #System 2 
  # =============
  # show the books to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  # Calculate recommendations when the sbumbutton is clicked
  df_system2 <- eventReactive(input$btn2, {
    withBusyIndicatorServer("btn2", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      shinyjs::logjs(user_ratings)
      
      j = user_ratings$MovieID
      x = user_ratings$Rating
      print(j)
      print(x)
      
      user_results = (1:10)/10
      user_predicted_ids = predict_cf(Rmat, rec_UBCF1, schema_1, j, x)
      recom_results <- data.table(Rank = 1:10, 
                                  MovieID = movies$MovieID[user_predicted_ids], 
                                  Title = movies$Title[user_predicted_ids], 
                                  Predicted_rating =  user_results)
      
    }) # still busy
    
  }) # clicked on button
  
  
  # display the recommendations
  output$results_system2 <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df_system2()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
}) # server function