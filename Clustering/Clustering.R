data("USArrests")
us_arrests<-USArrests #Making copy
head(us_arrests)
str(us_arrests)
summary(us_arrests)
sus_arrests<-scale(us_arrests)
head(sus_arrests)
dist_data<-dist(sus_arrests, method = 'euclidean')
hdata<-hclust(dist_data)
plot(hdata)
abline(h=3.75, lty=2) 

set.seed(123)
kus_arrests<-kmeans(sus_arrests, centers = 4, nstart = 50)
#Simple visualisation of clusters 
plot(x=sus_arrests[,1], y=sus_arrests[,2], col=kus_arrests$cluster)
points(kus_arrests$centers, pch=3, cex=2)

library(cluster)
clusplot(sus_arrests, kus_arrests$cluster, color = T, labels = 2, main = 'Cluster Plot')
plot(silhouette(kus_arrests$cluster, dist = dist_data), col=2:5)

PAMus_arrests<-pam(sus_arrests, 4)
clusplot(sus_arrests, PAMus_arrests$clustering, color = T, main = 'Cluster Plot')

kmax<-10
WSSus_arrests<-sapply(1:kmax, function(k) kmeans(sus_arrests, centers = k, nstart = 10)$tot.withinss)

plot(1:kmax, WSSus_arrests, type = 'b', xlab = 'k', ylab = 'Total wss')
abline(v=4, lty=2)

set.seed(123)
gap_stat <- clusGap(sus_arrests, FUN = kmeans, nstart = 25, K.max = 10, B = 50)

plot(gap_stat, xlab = "Number of clusters k")
abline(v = 4, lty = 2)

