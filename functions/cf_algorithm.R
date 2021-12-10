
# ============================================================
# Functions used in implementation of collaborative filtering.
# ============================================================


library(Matrix)
library(recommenderlab)
library(slam)
library(data.table)

ratings = read.csv(file = './data/ratings/ratings.dat', 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
ratings$Timestamp = NULL

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
#schema_1 = evaluationScheme(Rmat, method="split", train=0.8, given=15, k=1)

#Training the recommender. This and the previous steps need to be performed at the runtime of the app, as they all take around 3 minutes.
#rec_UBCF1 = Recommender(getData(schema_1, "train"), method = 'UBCF',
#                        parameter = list(normalize = 'Z-score', 
#                                         method = 'Cosine', 
#                                         k = 25))

predict_cf = function(Rmat, user_MovieID, Rating){
  set.seed(100)
  
  #Creating the test/training split
  schema_1 = evaluationScheme(Rmat, method="split", train=0.8, given=15, k=1)
  
  #Training the recommender. This and the previous steps need to be performed at the runtime of the app, as they all take around 3 minutes.
  rec_UBCF1 = Recommender(getData(schema_1, "train"), method = 'UBCF',
                          parameter = list(normalize = 'Z-score', 
                                           method = 'Cosine',
                                           weighted = TRUE,
                                           nn = 25))
  
  
  user_movieIDs = user_MovieID
  renamed_user_movieIDs = paste0('m', user_movieIDs)
  full_movieID = colnames(Rmat)
  new.ratings = rep(NA, length(full_movieID))
  new.ratings[1:4] = 1:4
  
  new.user = matrix(new.ratings,
                    nrow = 1, ncol = length(full_movieID),
                    dimnames = list(
                      user=paste('hugh'),
                      item=full_movieID
                    ))
  
  for (q in 1:length(renamed_user_movieIDs)){
    if(renamed_user_movieIDs[q]!= "m1" && renamed_user_movieIDs[q]!= "m2" && renamed_user_movieIDs[q]!= "m3" && renamed_user_movieIDs[q]!= "m4"){
      new.user[1,(renamed_user_movieIDs[q])] = Rating[q]
    }
  }
  
  new.Rmat = as(new.user, 'realRatingMatrix')
  
  #Instead, taking 1 user from existing data for test. When you will have real data, please substitute "getData(schema_1, "unknown")[2]" by your real Rating matrix.
  
  recom = predict(rec_UBCF1 , new.Rmat, type="ratings")
  recom_list=as(recom, "list")
  df=data.frame(film=rownames(as.data.frame(recom_list)), rating= as.matrix(as.data.frame(recom_list)[,1]))
  
  #10 recommended movies
  df[order(-df$rating),][1:10,]
  pred_id = rep(0, 10)
  for(i in 1:10){
    film_str_length = nchar(df[order(-df$rating),][i, 1])
    pred_id[i] = strtoi(substr(df[order(-df$rating),][i, 1], 2, film_str_length))
  }
  print(pred_id)
  pred_id
}

  
#(predict_cf(rec_UBCF1, schema_1))
