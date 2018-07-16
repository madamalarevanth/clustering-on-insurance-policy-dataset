#import the insurance claim dataset from the .csv file 
data1 <-read.csv("C:/Users/REVANTH/Desktop/data/cluster data.csv",header= TRUE,sep=",")

#to view the data 
View(data1)

#gives the dimensions of the data set
dim(data1)

#gives the names of the variables in the data set
names(data1)

#displays top 10 observations of the data set
head(data1)

#displays bottom top 10 observations of the data set
tail(data1)

#displays structure of the variables in the data set
str(data1)

#gives the summary of the variables of the data set 
summary(data1)

#to keep only selected variables from data( as we are having an excess colum indicating the row number we are deleting it )
data3<-data1[ ,c(2,5)]


# elbow method for kmeans clustering,to identify the no of cluster ####

### assigning maximal no of clusters 
k.max <- 15 

data3 <-data3

# it calculates the total within cluster sum of the squares for all the k values
wss <- sapply(1:k.max,function(k) {kmeans(data3,k)$tot.withinss})

#plotting the elbow curve to identify the optimal no of clusters 
plot (1:k.max,wss,type = "b" , frame = FALSE,
      xlab = "no of cluster k ",
      ylab = "total within cluster sum of square")

#from the above plotit is know that 3 clusters is optimum 
km<-kmeans(data3,3)

km
#if you look at the km 
#K-means clustering with 3 clusters of sizes 33, 33, 34

#Cluster means:
#     Age Income
#1 51.24242 152500
#2 41.60606  52000
#3 45.50000 102250

#package required to plot kmeans graph
library(animation)

#function for kmeans graph
kmeans.ani(data3,3)

# it gives the cluster vector of all observations
km$cluster

#withinss stands for within sum of squares i.e the total of the squared distance between a point
#and its clusters center,across all the points in the cluster.
#it captures inta-cluster variability
km$withinss

#tots.withinss stands for total of within sum of squares
#it is the roral of within sum of squares across all the clusters were added 
km$tot.withinss

#totss stands for total sum of squares
#it is the toal of squared distance between a point and the center for the entire data
km$totss

#betweenss stands for between the sum of squares 
#totss-tot.withinss
#it gives inter cluster variability
km$betweenss

finalclus<-cbind(data3,km$cluster)
View(finalclus)
