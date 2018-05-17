# Various similarity functions

## pearson similarity function

sim_function_pearson<- function(x,y){
  t1<-(x-mean(x, na.rm = T))
  t2<-(y-mean(y, na.rm = T))
  temp<-(t1*t2)%>%sum(na.rm = T)
  rated_entry_x <- !is.na(x)
  rated_entry_y <- !is.na(y)
  intersection_entries <- (rated_entry_x & rated_entry_y)
  t3<-sqrt(sum(t1[intersection_entries]^2,na.rm = T))*sqrt(sum(t2[intersection_entries]^2,na.rm = T))
  return(temp/t3)
}

## adjusted pearson
sim_function_conspear<- function(x,y){
  t1<-(x-median(x, na.rm = T))
  t2<-(y-median(y, na.rm = T))
  temp<-(t1*t2)%>%sum(na.rm = T)
  rated_entry_x<-!is.na(x)
  rated_entry_y<-!is.na(y)
  intersection_entries <- (rated_entry_x & rated_entry_y)
  t3<-sqrt(sum(t1[intersection_entries]^2,na.rm = T))*sqrt(sum(t2[intersection_entries]^2,na.rm = T))
  return(temp/t3)
}

## weighted pearson 

sim_function_weipear<- function(x,y ){
  rated_entry_x<-!is.na(x)
  rated_entry_y<-!is.na(y)
  intersection_entries <- (rated_entry_x & rated_entry_y)%>%sum()
  if(intersection_entries <= 50){
    sim_function_pearson(x,y)*intersection_entries/50
  } else {
    sim_function_pearson(x,y)
  }
}

## jaccard similarity function

sim_function_jaccard <- function(x,y){
  rated_entry_x<-!is.na(x)
  rated_entry_y<-!is.na(y)
  intersection_entries<- (rated_entry_x & rated_entry_y)%>%sum()
  union_entries<- (rated_entry_x | rated_entry_y)%>%sum
  return((intersection_entries/union_entries))
}

## cosine similarity function

sim_function_cosine<- function(x,y){
  rated_entry_x <- !is.na(x)
  rated_entry_y <- !is.na(y)
  intersection_entries <- (rated_entry_x & rated_entry_y)
  dotprod <- (x*y)%>%sum(na.rm = T)
  magnitude_prod <- sum(x[intersection_entries]^2, na.rm = T)%>%sqrt()*sum(y[intersection_entries]^2, na.rm = T)%>%sqrt()
  return(dotprod/magnitude_prod)
}

## adjusted cosine

sim_function_adjcosine<- function(x,y){
  x[is.na(x)] <- 0
  y[is.na(y)] <- 0
  dotprod<-((x-mean(x))*(y-mean(y)))%>%sum(na.rm = T)
  magnitude_prod <- (sum((x-mean(x))^2)%>%sqrt())*(sum((y-mean(y))^2)%>%sqrt())
  return(dotprod/magnitude_prod)
}

## Mean squared difference

sim_function_msd<- function(x,y){
  rated_entry_x<-!is.na(x)
  rated_entry_y<-!is.na(y)
  squared_sum_diff<- (((x-y)^2)%>%sum(na.rm = T))/(rated_entry_x&rated_entry_y)%>%sum()
  1-squared_sum_diff
}

## Mean measure of divergence

sim_function_mmd<- function(x,y){
  x_table<-x%>%table 
  y_table<- y%>%table
  
  x_table[which(!(1:5 %in% (x_table%>%names%>%as.numeric())))] = 0
  y_table[which(!(1:5 %in% (y_table%>%names%>%as.numeric())))] = 0
  
  (1+(((x_table-y_table)^2-(1/sum(x_table))-(1/sum(y_table)))/(!is.na(x))%>%sum())%>%sum(na.rm = T))^-1
}

## sigmoidpcc dist

sim_function_sigmpcc<- function(x, y){
  rated_entry_x<-!is.na(x)
  rated_entry_y<-!is.na(y)
  exp_temp<-(1+exp(-(rated_entry_x&rated_entry_y)%>%sum()/2))^-1
  sim_function_pearson(x,y)*exp_temp
}


similarity_function <- function(x,y,type){
  switch(type, 
         pearson = sim_function_pearson(x,y),
         cosine = sim_function_cosine(x,y),
         jaccard = sim_function_jaccard(x,y),
         adjcos = sim_function_adjcosine(x,y),
         sigmpcc = sim_function_sigmpcc(x,y),
         mmd  = sim_function_mmd(x,y),
         msd = sim_function_msd(x,y),
         conspear = sim_function_conspear(x,y),
         weipear = sim_function_weipear(x,y)
  )
}

