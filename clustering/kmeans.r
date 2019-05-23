#import library
library(cluster)
library(gridExtra)
library(factoextra)
library(tidyverse)

#import data
data(agriculture)
df<-agriculture

#data apa yang kita miliki
dim(df)
head(df,10)
ggplot(data = df, aes(x,y)) +
  geom_point()


#apakah ada data yang hilang
sum(is.na(df))

#apakah data perlu dinormalisasi
dfnorm <- scale(df)

#apakah data bisa digunakan untuk melakukan clustering
distance <- get_dist(dfnorm, method = "euclidean" )
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#proses clustering
k2 <- kmeans(dfnorm, centers = 2, nstart = 25)
k3 <- kmeans(dfnorm, centers = 3, nstart = 25)
k4 <- kmeans(dfnorm, centers = 4, nstart = 25)
k5 <- kmeans(dfnorm, centers = 5, nstart = 25)
k6 <- kmeans(dfnorm, centers = 6, nstart = 25)


p1 <- fviz_cluster(k2, geom = "point", data = dfnorm) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point", data = dfnorm) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point", data = dfnorm) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point", data = dfnorm) + ggtitle("k = 5")
p5 <- fviz_cluster(k6, geom = "point", data = dfnorm) + ggtitle("k = 6")
grid.arrange(p1, p2, p3, p4, p5, nrow = 2)

#mencari jumlah kluster yang optimal menggunakan Elbow
set.seed(123)
wss <- function(k) {
  kmeans(dfnorm, k, nstart = 10 )$tot.withinss
}
k.values <- 1:11
wss_values <- map_dbl(k.values, wss)
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


#mencari jumlah kluster yang optimal menggunakan Silhouette
avg_sil <- function(k) {
  km.res <- kmeans(dfnorm, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df))
  mean(ss[, 3])
}
k.values <- 2:11
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")
fviz_nbclust(dfnorm, kmeans, method = "silhouette")