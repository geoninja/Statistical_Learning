#Lab Chapter 10: Unsupervised Learning

#PCA
#Let's use the USArrests dataset from the base R package.
dimnames(USArrests)
dim(USArrests)

#Let's briefly examine the data to determine whether standardization is required
apply(USArrests, 2, mean) #calculate mean of each column
apply(USArrests, 2, var) #we see Assault's variance is much larger than other features

#Let's use prcomp() for PCA
pca.out = prcomp(USArrests, scale = T) #scale makes mean = 0 and sd = 1
names(pca.out) #center and scale = means and sd used for scaling prior to PCA
pca.out$center; pca.out$scale #same as apply functions above

pca.out$rotation # matrix of principal components loadings
#We see there are 4 principal components (min(n-1, p))

dim(pca.out$x) #x is the matrix in which the columns are each principal component score vector

#Let's plot the first two principal components in a biplot
biplot(pca.out, scale = 0, cex = 0.7) #scale=0 means arrows are scaled to represent loadings

#Let's access the sd of each PC (decreasing for each kth PC, as expected)
pca.out$sdev
pca.var = pca.out$sdev ^ 2 #variance explained by each component
prop.var = pca.var / sum(pca.var) #% of variance explained per component

#Let's now plot the variance explained by each component, and the cumulative variance:
plot(prop.var, xlab = "Principal Component", ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

plot(cumsum(prop.var), xlab = "Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained", ylim = c(0, 1), type = "b")


#K-Means Clustering - kmeans()
#Let's create some data to work with, where the first 25 obs have a mean shift from the next 25
set.seed(2)
x = matrix(rnorm(50 * 2), ncol = 2)
x[1:25, 1] = x[1:25, 1] + 3 #shift of 3
x[1:25, 2] = x[1:25, 2] -4

#Let's now perform k-means clsutering with k = 2
km.out = kmeans(x, centers = 2, nstart = 20) #nstart = how many random sets should be chosen
km.out$cluster #assignments of the first 50 obs: perfect assignment!

#Let's plot the data according to its classification
#Here we can plot the observations because they're 2D, but if there were more dimensions we
#could maybe perform PCA and plot the first two principal components score vectors.
plot(x, col = (km.out$cluster + 1), main = "K-Means Clustering Results with K = 3",
     xlab = "", ylab = "", pch = 20, cex = 2)

#Let's check the results if we decide to use k = 3 for the same data
set.seed(4)
km.out = kmeans(x, 3, nstart = 20)
km.out

#Let's now check on the effect of nstart (random assignments)
set.seed(3)
km.out = kmeans(x, 3, nstart = 1)
km.out$tot.withinss #total within cluster sum of squares, which we want to minimize: 104.33

km.out = kmeans(x, 3, nstart = 20)
km.out$tot.withinss #97.97 
#conclusion: we should ALWAYS use a large value for nstart (such as 20 or 50), otherwise an
#undesirable local optimum may be obtained!

#Let's run one more example (from video), using fake data with 4 clusters:
set.seed(101)
mat = matrix(rnorm(100 * 2), 100, 2)
matmean = matrix(rnorm(8, sd = 4), 4, 2)
idx = sample(1:4, 100, replace = T)
mat = mat + matmean[idx, ]
plot(mat, col = idx, pch = 19)

km.out = kmeans(mat, 4, nstart = 15)
km.out
plot(mat, col = km.out$cluster, cex = 2, pch = 1, lwd = 2, main = "K-Means Clustering 
     Results")
points(mat, col = idx, pch = 19)
points(mat, col = c(4, 3, 2, 1)[idx], pch = 19) #we see there were only 2 misclassifications


#Hierarchical Clustering - hclust()
#Here we use the same fake data created for k-means clustering. We will plot dendograms for
#complete, single, and average linkage clustering, using the Euclidian distance as the
#dissimilarity measure. The dist() function computes the 50 x 50 inter-obs Euclidian distance
#matrix.
hc.complete = hclust(dist(x), method = "complete") #complete linkage
hc.average = hclust(dist(x), method = "average") #average linkage
hc.single = hclust(dist(x), method = "single") #single linkage

#Let's now plot the dendograms. The numbers at the bottom of the plot identify each observation
par(mfrow = c(1, 3))
plot(hc.complete, main = "Complete Linkage", xlab = "", sub = "", cex = 0.9)
plot(hc.average, main = "Average Linkage", xlab = "", sub = "", cex = 0.9)
plot(hc.single, main = "Single Linkage", xlab = "", sub = "", cex = 0.9)

#Let's determine the cluster labels for each obs associated with a given cut of the 
#dendogram (which can be specified by number of clusters k, or height h). For k = 2, we 
#cut the tree at level 2, and produce a vector of numbers (labels) of 1 and 2, i.e. which
#branch each observation is on.
cutree(hc.complete, k = 2)
cutree(hc.average, k = 2)
cutree(hc.single, k = 2) #not an appropriate choice of k for single linkage dendogram
#single linkage has identified one point as belonging to its own cluster. A more sensible
#result is obtained with k = 4, but there are stil two singletons
hc.cut = cutree(hc.single, k = 4)
table(hc.cut)
plot(hc.single, labels = hc.cut)

#Let's use the function scale() to scale the data before performing clustering:
xsc = scale(x) #mean = 0 and sd = 1
par(mfrow = c(1, 1))
plot(hclust(dist(xsc), method = "complete"), main = "Hierarchical Clustering with Scaled
     Features", xlab = "", sub = "")

#Lastly, we can compute the correlation-based distance using the as.dist() function, which
#converts an arbitrary square symmetric matrix into a form that the hclust function 
#recognizes as a distance matrix (this only makes sense with at least 3 dimensions)
x = matrix(rnorm(30 * 3), ncol = 3)
dd = as.dist(1- cor(t(x))) #t transposes the matrix
plot(hclust(dd, method = "complete"), main = "Complete Linkage with Correlation Based
     Distance", xlab = "", sub = "")


#Example on the NCI60 cancer cell line microarray
#6830 gene expression measurements on 64 cancer cell lines
library(ISLR)
nci.labs = NCI60$labs
table(nci.labs) #labels for 14 types of cancer, only used for testing unsupervised results
nci.data = NCI60$data #large matrix
dim(nci.data) #64 x 6830

#PCA on the NCI60 data (we scale the genes although one could reasonably argue that it's 
#better to not scale them)
pr.out = prcomp(nci.data, scale = T)

#Let's plot the first few principal components score vectors. 
#The cell lines (obs) corresponding to a given cancer type will be plotted in the same color,
#so we can see to what extent the observations within a cancer type are similar to each other

#First, let's create a function to assign a color to each element of a numeric vector (64 
#cell lines), based on the type of cancer to which it corresponds.
cores = function(vec){
    cols = rainbow(length(unique(vec)))
    return(cols[as.numeric(as.factor(vec))])
}
#rainbow() takes a positive integer as argument and returnsa vector containing a number of
#distinct colors

par(mfrow = c(1, 2))
plot(pr.out$x[, 1:2], col = cores(nci.labs), pch = 19, xlab = "Z1", ylab = "Z2")
plot(pr.out$x[, c(1, 3)], col = cores(nci.labs), pch = 19, xlab = "Z1", ylab = "Z3")
#pr.out$x are the scores for each component (64 x 64 matrix, in this case)
#we see from the plots that cell lines corresponding to a single cancer type do tend to have
#similar values in the first few principal component score vectors. This indicates that
#cell lines from the same cancer type tend to have pretty similar gene expression levels.

#Let's check the summary with the proportion of variance explained, and plot it as well.
summary(pr.out)
plot(pr.out) #this plots the variance, or the square of pr.out$sdev

pve = 100 * pr.out$sdev ^ 2/sum(pr.out$sdev ^ 2)
par(mfrow = c(1, 2))
#plot of pve for each principal component (scree plot)
plot(pve, type = "o", ylab = "PVE", xlab = "Principal Component", col = "blue")
#plot of cummulative pve for each principal component
plot(cumsum(pve), type = "o", ylab = "PVE", xlab = "Principal Component", col = "brown3")

#Note: pve can also be directly computed by:
summary(pr.out)$importance[2, ]
summary(pr.out)$importance[3, ] #cumsum


#Clustering the observations of the NCI60 data
#Do the observations cluster into distinct types of cancer?

#Let's scale (optional, as mentioned earlier) the data for hierarchical clustering (sd=1)
sd.data = scale(nci.data)

#Clustering using complete, single and average linkage, and Euclidian distance dissimilarity
par(mfrow = c(1, 3))
data.dist = dist(sd.data)

plot(hclust(data.dist), labels = nci.labs, main = "Complete Linkage", xlab = "", sub = "",
     ylab = "")

plot(hclust(data.dist, method = "average"), labels = nci.labs, main = "Average Linkage", 
     xlab = "", sub = "", ylab = "")

plot(hclust(data.dist, method = "single"), labels = nci.labs, main = "Single Linkage", 
     xlab = "", sub = "", ylab = "")

#Clearly, from the plots, cell lines tend to cluster together, although the clustering is
#not perfect.

#Let's now cut the complete linkage dendogram at, say, 4 clusters:
hc.out = hclust(dist(sd.data))
hc.out
hc.clusters = cutree(hc.out, 4)
table(hc.clusters, nci.labs) #there are some clear patterns: all leukemia cell lines fall in
#cluster 3, while breast cancer cell lines are spread out over three different clusters.
#Let's plot the cut on the dendogram that produces these 4 clusters:
par(mfrow = c(1, 1))
plot(hc.out, labels = nci.labs)
abline(h = 139, col = "red") #h = height

#How does K-means clustering perform with the same number of 4 clusters? We expect it to be
#very different from hierarchical clustering. Let's check:
set.seed(2)
km.out = kmeans(sd.data, 4, nstart = 20)
km.clusters = km.out$cluster
table(km.clusters, hc.clusters)
#We see that the 4 clusters by each method are somewhat different. Cluster 2 in K-means is
#identical to cluster 3 in hierarchical clustering. However, the other clusters differ.
#For example: cluster 4 in K-means contains a portion of the observations assigned to cluster
#1 by hc, and all the observations assigned to cluster 2 by hc.

#Let's now perform hc in the first few principal components score vectors, rather than the
#entire data matrix:
hc.pca = hclust(dist(pr.out$x[, 1:7]))
plot(hc.pca, labels = nci.labs, main = "Hier Clust on First Seven Score Vectors")
table(cutree(hc.pca, 4), nci.labs)
#As expected, the results are different. PCA "de-noises" the data.

#Let's check the results using the same PCA with K-means, for K = 4
set.seed(100)
km.pca = kmeans(pr.out$x[, 1:7], 4, nstart = 20)
km.clust = km.pca$cluster
table(km.clust, cutree(hc.pca, 4))
plot(pr.out$x[, 1:7], col = (km.pca$cluster + 1), xlab = "", ylab = "", pch = 20, cex = 2,
     main = "K-Means Clust on First Seven Score Vectors")

#Solutions to Quiz 10R

load("10.R.RData")
#range(x.test)

#Question1
dfx = rbind(x, x.test)
pcax = prcomp(dfx, scale = T)
pcax.var = pcax$sdev ^ 2 #variance explained by each component
cumsum(pcax.var[1:5]) #69.9712

#vars = prcomp(rbind(x,x.test),scale=TRUE)$sdev^2
#cumsum(vars[1:5]) #69.9712 same as above

#Question 2
mod1 = lm(y ~., data = as.data.frame(cbind(pcax$x[1:300, 1:5], y)))
mod1_pred = predict(mod1, newdata = as.data.frame(pcax$x[301:1300, 1:5]))
mse_mod1 = mean((mod1_pred - y.test)^2) #0.9922779

#Question 3
train = data.frame(x, y)
lm_fit = lm(y ~., data = train) #ordinary least squares with all components
lm_pred = predict(lm_fit, newdata = x.test)
mse_lm = mean((lm_pred - y.test)^2) #3.657197

library(pls) #partial least squares with 5 components
pcr_fit = pcr(y ~., data = train, scale = TRUE)
pcr_pred = predict(pcr_fit, newdata = x.test, ncomp = 5)
mse_pcr = mean((pcr_pred - y.test)^2) #0.9966097




