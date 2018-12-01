data(mtcars)
dimnames(mtcars)
mtcars.scale=scale(mtcars) # Normalizing all fields of mtcars
head(mtcars.scale)

dist.res=dist(mtcars.scale, method='euclidean')
dist1=dist(mtcars.scale,method = "manhattan", diag = TRUE)
dist2=dist(mtcars.scale,method = "canberra", upper = TRUE)

#Hierarchical clustering results
hc=hclust(dist.res, method="complete")
hc
plot(hc)

# H Clustering using centroid
hc1=hclust(dist.res, method="centroid",members = c(10,12,10))
hc1


# Visualization of hclust
plot(hc, labels=FALSE, hang=-1)
# Add rectagle around 4 groups
ssd<-rect.hclust(hc, k=4, border=2:5) #border specifies colours of the border (2:5=2,3,4,5)
cs<-cutree(hc,k = 4) #use h for specifying height & k for number of clusters
cs[order(cs)]
cs1<-cutree(hc,h= 3)

# Reversals
plot(hc1)


# Kmeans

# cluster results

set.seed(20)

# check for collinearity because k means can suffer from multicollinerity 
library(usdm)
vifstep(mtcars.scale,th=10)

# Kmeans algo assuming K = 4 all factors
#note that we are not using distance values because kmeans() calculates it on its own
mtcarsCluster= kmeans(mtcars.scale, 4)
mtcarsCluster

# removing collinear variables
mtcars_upd=mtcars.scale[,-c(2,3)]
# assumed K = 4 but without collinear variables
mtcarsClusternew= kmeans(mtcars_upd, 4)
mtcarsClusternew

# Ideal K using elbow graph

wss=(nrow(mtcars.scale)-1)*sum(apply(mtcars.scale,2,var))#(no of rows-1)*sum(variance of each column)
#apply will apply the 3rd argument over the 2nd argument(which is 1 or 2 where 1=rows,2=col) across the dataset

for(i in 2:15)
{
  wss[i]=sum(kmeans(mtcars.scale, centers=i)$withinss)
}

set.seed(20)

plot(1:15,wss, type="b", xlab="No. of Clusters", ylab="Within Group Sum of Squares ")

fitted(mtcarsCluster, method = "classes")
Results= cbind.data.frame(mtcars,cluster=mtcarsCluster$cluster)
Results[order(Results$cluster),]
