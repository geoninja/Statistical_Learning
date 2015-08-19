#Lab Chapter 9: Support Vector Machines

library(e1071)
#example of function used in this lab: expand.grid()
m = expand.grid(height = seq(60, 80, 5), weight = seq(100, 300, 50), sex = c("Male", "Female"))

#Let's generate some data to work with
set.seed(10111)
x = matrix(rnorm(40), 20, 2) #40 random, normaly distributed values in 20 rows and 2 cols
mean(x[11:20, 1]); mean(x[11:20, 2]) #mean is "0" (-0.203, 0.283)

y = rep(c(-1, 1), c(10, 10)) #vector with 10 each, values 1 and -1
x[y == 1, ] = x[y == 1, ] + 1 #for all y=1, change mean from 0 to 1 for each coordinate.
mean(x[11:20, 1]); mean(x[11:20, 2]) #mean changed to "1" (0.796 and 1.283)
mean(x[1:10, 1]); mean(x[1:10, 2]) #these have not changed

plot(x, col = y + 3, pch = 19) #col can also be numeric, data seems linearly separable
data = data.frame(x, y = as.factor(y)) #create df to train model, turning y into factor type
#Note: for this library, y MUST be turned into a factor for classification (not regression)

#LINEAR SVM

#Let's fit the SV classifier model. 
#Cost (c) is a tuning parameter. Here we just use 10. 
#Scale = FALSE will NOT standardize the variables
svcfit = svm(y ~., data = data, kernel = "linear", cost = 10, scale = F)
print(svcfit) # 6 support vectors
plot(svcfit, data) #package's plot function

#Let's make our own plot for the fitted data
#First, we need to make a lattice (grid) of values for X1 and X2. The function below
#produces the coordinates of n * n points to a lattice covering the domain of x.
make.grid = function(x, n = 75){
    g_range = apply(x, 2, range)
    x1 = seq(from = g_range[1, 1], to = g_range[2, 1], length = n) #n values uniformly spaced
    x2 = seq(from = g_range[1, 2], to = g_range[2, 2], length = n)
    expand.grid(X1 = x1, X2 = x2) #this makes lattice for the range of x1 and x2 values
}
xgrid = make.grid(x)
head(xgrid, 10); tail(xgrid, 10)

#Then, we make a prediction at each point on the lattice
ygrid = predict(svcfit, xgrid)
svcfit$index #the support points are indexed in the $index component of the fit.
str(svcfit)

#Finally, we plot the lattice, color-coded according to the classification (ygrid). 
#We can now see the classification boundary.
plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], pch = 20, cex = 0.2, main = "c = 10")
points(x, col = y + 3, pch = 19)
points(x[svcfit$index, ], pch = 5, cex = 2) #We use a larger diamond shape to differentiate 
#the support points. They're either on the margin, or on the wrong side of the margin.

#Now we're going to use a formula to extract the coefficients (this only makes sense for
#a linear kernel). Then, using algebra, we include the decision boundary and the margins.
beta0 = svcfit$rho
beta = drop(t(svcfit$coefs) %*% x[svcfit$index, ])
beta0; beta

abline(beta0 / beta[2], -beta[1] / beta[2], lty = 2) #decision boundary
abline((beta0 - 1) / beta[2], -beta[1] / beta[2], lty = 2) #upper margin
abline((beta0 + 1) / beta[2], -beta[1] / beta[2], lty = 2) #lower margin

#We used a given value for c, the margin budget. Let's use tune() to select this parameter.
#Note: by default, tune() will do 10 fold cross-validation.

tune.out = tune(svm, y~., data = data, kernel = "linear", 
                ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
#the above indicates we want to compare SVMs (can take other methods, too) with a linear
#kernel, using a range of cost parameter (note the use of list).
summary(tune.out) #cost = 100 results in the lowest CV error.
#The tune function stores the best model, which can be accessed as such:
bestmod = tune.out$best.model
summary(bestmod)


#Let's work with another simulated dataset in which two classes are barely linearly separable
set.seed(1)
x = matrix(rnorm(40), ncol = 2)
y = c(rep(-1, 10), rep(1, 10))
x[y == 1, ] = x[y == 1, ] + 0.5
plot(x, col = (y + 5)/2, pch = 19)
data = data.frame(x, y = as.factor(y))

svcfit = svm(y ~., data = data, kernel = "linear", cost = 1e5)
summary(svcfit)
plot(svcfit, data)



#NON-LINEAR SVM

load(url("http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/ESL.mixture.rda"))
names(ESL.mixture) #we will use this simulated data for the non-linear SVM fit.
#x and y are training data, px1 and px2 are grid values
rm(x, y) #clear environment from previous objects with same name
attach(ESL.mixture)

plot(x, col = y + 1)
dat = data.frame(y = factor(y), x)

#Let's fit the model using a radial kernel
fit = svm(factor(y) ~., data = dat, scale = F, kernel = "radial", cost = 5)
summary(fit)
plot(fit, dat)

#Let's now create the grid (as before) to make predictions on. We don't need to use the
#function we created previously because the data set comes with the grid points.
xgrid = expand.grid(X1 = px1, X2 = px2)
ygrid = predict(fit, xgrid)
plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = 0.2) #we see non-linear boundary
points(x, col = y + 1, pch = 19)

#Let's use tune() to choose the best values for gamma (radial kernel) -BOOK ADDITIONAL
tune.out = tune(svm, y ~., data = dat, scale = F, kernel = "radial", cost = 5,
                ranges = list(gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out) #best gamma = 1
table(actual = dat$y, pred = predict(tune.out$best.model, newx = xgrid))
error_rate = 31/200 #0.155

#Let's add the actual decision boundary to the plot
#The variable prob in the dataframe is the true probability of class 1 for the grid points.
#By plotting the 0.5 contour, we obtain the Bayes Decision Boundary (the best we can obtain)

func = predict(fit, xgrid, decision.values = T) #predict the fit on the grid
#decision.values = T gets the actual function, not just the classification
func = attributes(func)$decision #access the decision attribute

length(px1); length(px2) #69 and 99, respectively
contour(px1, px2, matrix(func, 69, 99), level = 0, add = T) #fit decision boundary (level 0)

#Bayes decision boundary: actual prob, level = 50% (simulated data)
contour(px1, px2, matrix(prob, 69, 99), level = 0.5, add = T, col = "blue", lwd = 2) 

#Conclusion: by comparing both boundaries, we see that the radial kernel did a very good job.


#ROC CURVES

library(ROCR)


