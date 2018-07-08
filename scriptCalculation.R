
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
pred_matrix_func<- function(nn = c(2,3,4), similarityMatrix = sim_matrix, ratingMatrix = ratingmat, itemBased = itemItem){
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
##############################
##### MAE error function #####
##############################



uu_error_list = list()
ii_error_list = list()

# pearson left 20 ,50,100

# enter desired types from cosine, jaccard, adjcos, sigmpcc, mmd, msd, conspear
# adjust nearest_neighbours


ratingmatNAomitted <- ratingmat%>%
  as.vector()%>%
  na.omit()%>%
  as.factor()

notNAindexes<- !is.na(ratingmat)

types = c("cosine", "pearson", "adjcos", "weipear")

for(i in types){
  nearest_neigbours = c(3,5,10,20,50)
  for(itemItem in c(T)){
    sim_matrix<- sim_mat_func(ratingmat, itemItem, i,  F)
    prediction_matrix <- pred_matrix_func(nn = nearest_neigbours, similarityMatrix = sim_matrix, ratingMatrix = ratingmat, itemBased = itemItem)
    if(itemItem == T){
      ii_error_list[[i]] = lapply(prediction_matrix, function(x){error_function(ratingmat = ratingmat, prediction_matrix = x)})
      print(i)
      print("itemItem")
    } else {
      # uu_error_list[[i]] = lapply(prediction_matrix, function(x){caret::confusionMatrix(x[notNAindexes]%>%
      #                                                                                     rangeOptimize()%>%
      #                                                                                     as.factor(),
      #                                                                                   ratingmatNAomitted
      # )}) #
      print(i)
      print("userUser")
    }
  }
}

# errordf_uu<- data.frame(Method = replicate(length(nearest_neigbours), types)%>%t()%>%as.vector(), 
#                         NN = rep(nearest_neigbours, length(types)),
#                         MAE = (uu_error_list%>%unlist())[grepl("MAE", x = {uu_error_list%>%unlist()%>%names()})],
#                         RMSE = (uu_error_list%>%unlist())[grepl("RMSE", x = {uu_error_list%>%unlist()%>%names()})],
#                         row.names = NULL
# )
errordf_ii<- data.frame(Method = replicate(length(nearest_neigbours), types)%>%t()%>%as.vector(), 
                        NN = rep(nearest_neigbours, length(types)),
                        MAE = (ii_error_list%>%unlist())[grepl("MAE", x = {ii_error_list%>%unlist()%>%names()})],
                        RMSE = (ii_error_list%>%unlist())[grepl("RMSE", x = {ii_error_list%>%unlist()%>%names()})],
                        row.names = NULL
)


rangeOptimize <- function(x){
  x[x>=5] = 5
  x[x<=1] = 1
  x = round(x)
  x
}
