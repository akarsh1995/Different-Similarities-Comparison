
######################################
##### similarity matrix function #####
######################################
sim_mat_func <- function(ratingMatrix = ratingmat, itemBased = F, type_of_sim = "cosine", combinewithHM = F){
  types = type_of_sim
  ratingmat<- ratingMatrix
  ## calculating logical matrix (itemrated or not rated by user)
  if(itemBased==T){
    ratingmat<- ratingmat%>%t()
  }
  
  # ratingmat_logical <- !is.na(ratingmat)
  neighbours_to_calc<- list()
  
  n_users<- nrow(ratingmat)
  
  ## neighbours to take into consideration for each user
  for(i in 1:n_users){
    neighbours_to_calc[[i]] = seq(1,n_users)
  }
  
  ## calculation of various similarities defined in the "type" vector
  common_similarity_list <- list()
  
  for(i in types){
    
    top_similar_users<- list()
    for(user in seq_along(neighbours_to_calc)){
      temp_array<- array()
      
      for(kth in neighbours_to_calc[[user]]){
        temp<- similarity_function(ratingmat[user, ], ratingmat[kth,], i)
        temp_array[kth] = temp
      }
      top_similar_users[[user]]=temp_array
    }
    common_similarity_list[[i]] = top_similar_users
  }
  
  common_similarity_list<- lapply(common_similarity_list, function(x){
    for(i in 1:length(x)){
      x[[i]][i] = NA
    }
    x
  })
  
  # calculating mean of different similarities calculated previously
  common_similarity_list_hm<- list()
  
  for(j in seq_along(common_similarity_list[[1]])){
    temp_list<- list()
    for(type_length in 1:length(types)){
      temp_list[[type_length]] = common_similarity_list[[type_length]][[j]]
    }
    if(combinewithHM){
      common_similarity_list_hm[[j]] = do.call("rbind", temp_list)%>%apply(2, function(x){x<-mean(x);x[is.na(x)]<-NA;x})
    } else {
      library(psych)
      common_similarity_list_hm[[j]] = do.call("rbind", temp_list)%>%apply(2, function(x){x<-harmonic.mean(x);x[is.na(x)]<-NA;x})
    }
  }
  
  singleton_similarity = common_similarity_list_hm
  
  ## combined similarity matrix
  sim_matrix<-do.call("rbind", singleton_similarity)
  return(sim_matrix)
}

######################################
##### prediction matrix function #####
######################################
pred_matrix_func<- function(nn = c(5,10,15,20), similarityMatrix = sim_matrix, ratingMatrix = ratingmat, itemBased = itemItem){
  if(itemBased==T){
    ratingMatrix<- ratingMatrix%>%t()
  }
  top_similar_users<- list()
  for(i in 1:nrow(ratingMatrix)){
    top_similar_users[[i]]= similarityMatrix[i,]%>%
      order(decreasing = T)
  }
  
  ## mean centered prediction
  predicted_ratings_meanc <- list()
  prediction_list<- list()
  for(nearest_neigbours in nn){
    for(u in seq_along(top_similar_users)){
      user_mean = ratingMatrix[u,]%>%mean(na.rm = T)
      j = nearest_neigbours #nearest neighbors
      k_users <- top_similar_users[[u]][1:j] # k users indexes
      sim_u_v = similarityMatrix[u,k_users] # similarity array
      rvj_uv = ratingMatrix[k_users,] - rowMeans(ratingMatrix[k_users,], na.rm = T)
      numerator<-(sim_u_v*rvj_uv)%>%colSums(na.rm = T)
      denominator<- sim_u_v%>%abs()%>%sum()
      predicted_ratings_meanc[[u]] = (numerator/denominator)+ user_mean
    }
    prediction_matrix<-do.call(what = "rbind", predicted_ratings_meanc)
    if(itemBased == T){
      prediction_matrix<- (prediction_matrix%>%t())
    } else {
      prediction_matrix<- (prediction_matrix)
    }
    prediction_list[[length(prediction_list)+1]] = prediction_matrix
  }
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

types = c("cosine", "pearson")

for(i in types){
  nearest_neigbours = c(3)
  for(itemItem in c(F, T)){
    sim_matrix<- sim_mat_func(ratingmat, itemItem, i,  F)
    prediction_matrix <- pred_matrix_func(nn = nearest_neigbours, similarityMatrix = sim_matrix, ratingMatrix = ratingmat, itemBased = itemItem)
    if(itemItem == T){
      ii_error_list[[i]] = lapply(prediction_matrix, function(x){caret::confusionMatrix(x[notNAindexes]%>%
                                                                                          rangeOptimize()%>%
                                                                                          as.factor(),
                                                                                        ratingmatNAomitted
      )})
      print(i)
      print("itemItem")
    } else {
      uu_error_list[[i]] = lapply(prediction_matrix, function(x){caret::confusionMatrix(x[notNAindexes]%>%
                                                                                          rangeOptimize()%>%
                                                                                          as.factor(),
                                                                                        ratingmatNAomitted
      )}) #lapply(prediction_matrix, function(x){error_function(ratingmat = ratingmat, prediction_matrix = x)})
      print(i)
      print("userUser")
    }
  }
}

errordf_uu<- data.frame(Method = replicate(length(nearest_neigbours), types)%>%t()%>%as.vector(), 
                        NN = rep(nearest_neigbours, length(types)),
                        MAE = (uu_error_list%>%unlist())[grepl("MAE", x = {uu_error_list%>%unlist()%>%names()})],
                        RMSE = (uu_error_list%>%unlist())[grepl("RMSE", x = {uu_error_list%>%unlist()%>%names()})],
                        row.names = NULL
)
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
