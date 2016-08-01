library(RSQLite)
library(class)



conn <- dbConnect(SQLite(),"pitching.sqlite3")
pitch_quer <- dbSendQuery(conn,"select pitch_type, spin_rate, 
                      spin_dir, pfx_x, pfx_z,
                      break_angle, break_length
                      FROM pitch WHERE tfs_zulu LIKE '%2015%'
                      AND spin_rate IS NOT NULL 
                      AND start_speed IS NOT NULL 
                      AND end_speed IS NOT NULL
                      AND spin_dir IS NOT NULL
                      AND break_angle IS NOT NULL
                      AND break_length IS NOT NULL
                      LIMIT 25000
                      ")

pitch <- dbFetch(pitch_quer, n = -1)
dbClearResult(pitch_quer)
pitch_df <- as.data.frame(pitch)

#knn needs the categories to be in numeric form, we change the pitch type 
#from Factor to an index value
pitch_type_names <- unique(pitch_df$pitch_type)
for (i in 1:length(unique(pitch_df$pitch_type))){
  pitch_df$pitch_type[pitch_df$pitch_type==unique(pitch_df$pitch_type)[i]] <- i
}

#Create a normalizing function using feature scaling
norm_funct <- function(x){
  norm_val = (x - min(x)) /(max(x) - min(x))
  return(norm_val)
}

#normalize each value we are using.
measurements <- list("spin_rate", "pfx_x", "pfx_z", "break_angle", "break_length", "spin_dir")

for (m in measurements) {
  pitch_df[[m]]<- norm_funct(pitch_df[[m]])
}


set.seed(120)



#create an empty data frame in order to evaluate success of algorithm
pitch_knn <- data.frame()

#run algorithm with training set sizes ranging from 100 to 1000 increment by 100
for (n in seq(100,1000,100)){
  train <-1:n
  train.pitch_df <- pitch_df[train,]
  test.pitch_df <- pitch_df[-train,]
  
  train.def <- pitch_df$pitch_type[train]
  test.def <- pitch_df$pitch_type[-train]
#run algorithm for k ranging from 1 to 100  
  for (i in 1:25) {
    knn_res <- knn(train.pitch_df, test.pitch_df, train.def, k = i)
    pitch_knn <- rbind(pitch_knn, c(i, n, 100 * sum(test.def == knn_res)/ length(knn_res) ))
  }
}

colnames(pitch_knn) <- c("k", "training.size", "accuracy.per")


