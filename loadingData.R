library(tidyr)
library(dplyr)
library(readr)
library(proxy)

## task : paper on survey of neighbourhood (memory based) 
### Load user data from 100k movielens dataset
u.data <- read.delim("ml-100k/u.data", header=FALSE, stringsAsFactors=FALSE)
names(u.data)<-c("item id", "user id", "rating","timestamp")
u.data<- u.data[,-4]
ratingmat<-spread(data = u.data, "item id", "rating")%>%as.matrix()
ratingmat<-ratingmat[,-1]%>%t
dim(ratingmat)

# dataset for testing algorithm
ratingmat<- matrix(
  c(7,6,NA,1,1,6,7,3,2,NA,7,NA,3,2,1,4,4,1,3,2,5,3,1,3,3,4,4,NA,4,3),nrow = 5,byrow = F)

