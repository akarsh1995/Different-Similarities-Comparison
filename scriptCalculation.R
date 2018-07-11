
######################################
##### similarity matrix function #####
######################################
sim_mat_func <- function(ratingMatrix = ratingmat, itemBased = F, type_of_sim = "cosine"){
  ## calculating logical matrix (itemrated or not rated by user)
  if(itemBased==T){
    ratingMatrix<- ratingMatrix%>%t()
  }
 simil(ratingMatrix, similarity_function, type = type_of_sim)%>%as.matrix()
}

######################################
##### prediction matrix function #####
######################################
pred_matrix_func<- function(nn = c(5,10,20,30,50,70,100,150,200), similarityMatrix = sim_matrix, ratingMatrix = ratingmat, itemBased = itemItem){
  if(itemBased==T){
    ratingMatrix<- ratingMatrix%>%t()
  }
  top_similar_users<-apply(sim_matrix, 2, function(x, nn){order(x, decreasing = T)[1:nn]}, nn = max(nn))
  ## prediction calc
  prediction_list = list()
  for(n_near in seq_along(nn)){
    top_similar_users_chop <- top_similar_users[1:nn[n_near],]
    
    prediction_list[[n_near]]<-foreach(user = ratingMatrix%>%t, top_k_ith = top_similar_users_chop, sim = similarityMatrix, .combine = rbind) %dopar% {
      topkmatrix <- ratingMatrix[top_k_ith,]
      topkmatrix_rowmeans_diff <- topkmatrix - rowMeans(topkmatrix, na.rm = T)
      topk_sim_matrix <- sim[top_k_ith]
      numerator<-(topk_sim_matrix*topkmatrix_rowmeans_diff)%>%colSums(na.rm = T)
      denominator<-topk_sim_matrix%>%abs()%>%sum()
      numerator/denominator + user%>%mean(na.rm = T)
    }
  }
  
  if(itemBased==T){
    prediction_list <- prediction_list%>%lapply(t)
  }
  ## for each user we have to calculate the following
  names(prediction_list) = as.character(nn)
  return(prediction_list)
}


##############################
##### MAE error function #####
##############################
error_function<- function(ratingmat=ratingmat, prediction_matrix = prediction_matrix){
  error <- ratingmat-prediction_matrix
  MAE<- (error)%>%abs()%>%mean(na.rm = T)
  RMSE<-sqrt(mean(error^2, na.rm = T))
  return(list(MAE = MAE, RMSE = RMSE))
}


# enter desired types from cosine, jaccard, adjcos, sigmpcc, mmd, msd, conspear
# adjust nearest_neighbours

types = c("cosine", "pearson")

filename <- paste0("results ",timestamp(prefix = "", suffix = "",quiet = T),".csv")
res<-file(filename)
open(res, "wr")


  for(i in types){
    nearest_neigbours = c(5,10,20,30,50,70,100,150,200)
    for(itemItem in c(T, F)){
      sim_matrix<- sim_mat_func(ratingmat, itemItem, i)
      prediction_matrix <- pred_matrix_func(nn = nearest_neigbours, similarityMatrix = sim_matrix, ratingMatrix = ratingmat, itemBased = itemItem)
      df.list <- lapply(prediction_matrix, function(x){error_function(ratingmat = ratingmat, prediction_matrix = x)})%>%list()%>%`names<-`(i)
      df <- lapply(df.list, bind_rows, .id = "nn")%>%
        bind_rows(.id = "Method")
      
      if(itemItem == T){
        df <- cbind(Approach = "IBCF", df)       # creating dataframe of item-item errors of specific similarity approach
        cat(i, "IBCF-Approach completed\n")
      } else {
        df <- cbind(Approach = "UBCF", df)       # creating dataframe of item-item errors of specific similarity approach
        
        cat(i, "UBCF-Approach completed\n")
      }
      write_csv(df, res, col_names = F)
    }
    if(i == types[length(types)]) close(res)
  }
  
