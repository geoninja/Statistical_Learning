#Lab Chapter 5: RESAMPLING METHODS

#VALIDATION SET APPROACH
#library(ISLR)
#Dataset: Auto
dim(Auto) #392 by 9
View(Auto)

#Set aside training set (50% of dataset)
set.seed(1) #needed due to randomness of sampling subset
train = sample(392, 196) #ramdom subset of half of the observations, same as sample(1:392, 196)

#Model using linear regression to predict mpg from HP data
lm.fit = lm(mpg ~ horsepower, data = Auto, subset = train)

#Using predict() to estimate the response for all 392 observations, and
#mean() to calculate the MSE (via formula) of the 196 observations in the validation set.
attach(Auto)
mean((mpg - predict(lm.fit, Auto))[-train] ^ 2) #MSE formula, returns 26.14 (want to minimize)

#Note on MIT's course:
#vec = (mpg - predict(lm.fit, Auto))[train] #note residuals for training data, not test data
#head(vec) #same as: head(lm.fit$residuals)

#Let's try polynomial fitting to check if MSE can be minimized
lm.fit2 = lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train) #only one predictor
mean((mpg - predict(lm.fit2, Auto))[-train] ^ 2) #returns 19.82

lm.fit3 = lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train) #only one predictor
mean((mpg - predict(lm.fit3, Auto))[-train] ^ 2) #returns 19.78

#For comparison, let's choose a different training set
set.seed(2)
train = sample(392, 196)
lm.fit = lm(mpg ~ horsepower, data = Auto, subset = train)
mean((mpg - predict(lm.fit, Auto))[-train] ^ 2) #returns 23.3

lm.fit2 = lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train) #only one predictor
mean((mpg - predict(lm.fit2, Auto))[-train] ^ 2) #returns 18.9

lm.fit3 = lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train) #only one predictor
mean((mpg - predict(lm.fit3, Auto))[-train] ^ 2) #returns 19.26

#Conclusion based on validation set error rates: a model that predicts mpg using a quadratic
#function of horsepower performs better than a model that involves only a linear function of
#horsepower, but there's little evidence in favor of a model that uses a cubic function 
#over a squared one.


#LEAVE ONE OUT CROSS VALIDATION (LOOCV) APPROACH
#function cv.glm() will compute LOOCV for any GLM model, including linear regression if
#the argument "family" is not passed (then glm() output is the same as lm()). The function
#cv.glm() is part of the boot library.

library(boot)
glm.fit = glm(mpg ~ horsepower, data = Auto) #linear regression, output identical to lm()
cv.err = cv.glm(Auto, glm.fit) #K defaults to n, or LOOCV
names(cv.err) #returns "call", "K", "delta", "seed"
cv.err$delta #it takes a while... returns two identical values for LOOCV: 24.23
#delta is a vector with length 2, the raw CV estimate of prediction error, and the adjusted
# CV estimate designed to compensate for the bias introduced by not using LOOCV.

#Repeat LOOCV procedure to compare with incresingly complex polynomial fits:
#NOTE: computation is slow as cv.glm() iterates n times and makes no use of formula in the book
cv.error = rep(0, 5) #we will run 5 fits
for(i in 1:5){
    glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
    cv.error[i] = cv.glm(Auto, glm.fit)[[3]][1] #using index is more efficient than $delta
}

cv.error #sharp drop in CV error from linear (24.23) to quadratic (19.25), but no clear 
#improvement for higher order polynomials.


#K-FOLD CROSS VALIDATION
#Use the same cv.glm() function, this time specifying the number of folds k. 
#Computation is faster since it iterates only k times.
#Generating a vector with errors for 10 polynomial fits using k = 10
set.seed(10) #set seed since k-fold splits will be random
cv.error.10 = rep(0,10)
for(i in 1:10){
    glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
    cv.error.10[i] = cv.glm(Auto, glm.fit, K=10)[[3]][1] #only non-adjusted value*    
}
cv.error.10 #again sharp drop from linear to quadratic, but no clear evidence of improvement
#for higher-order polynomials. *but now they may differ slightly after bias correction.

#Comparing CV for different values of k (within optimal bias-variance trade-off range)
set.seed(5) #set seed since k-fold splits will be random
cv.error.k = rep(0,6) #for k = 5:10
for(i in 5:10){
    glm.fit = glm(mpg ~ poly(horsepower, 2), data = Auto) #we will use quadratic fit only
    cv.error.k[i-4] = cv.glm(Auto, glm.fit, K=i)[[3]][1] #only non-adjusted value   
}
cbind(k=5:10, cv.error.k) #for k=5, CV error is similar to LOOCV (19.26) for this dataset
cv.error.min = min(cv.error.k) #returns 19.18 for k = 8, but differences are small
diff = max(cv.error.k) - cv.error.min #returns 0.153


##BOOTSTRAP APPROACH

#Estimating the accuracy of a statistic of interest
#Dataset: Portfolio (100 by 2) from ISLR library
#Goal:estimate the optimal fraction to invest in each asset X and Y to minimize investment
#risk (alpha) of the combined portfolio. Use bootstrap to calculate SE of (alpha) estimate.

alpha.fn = function(data, index){
    X = data$X[index]
    Y = data$Y[index]
    return((var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2 * cov(X, Y))) #function in the book
}

alpha.fn(Portfolio, 1:100) #call alpha estimate based on all 100 values: returns 0.576

#use bootstrap by one random sampling with replacement from index 1:100, size 100, 
#to recompute an estimate of alpha
set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace = T)) #returns 0.596

#The boot() function repeats bootstrap sampling a R number of replicates
#boot() arguments are data, statistic, R, ... data can be vector, matrix or dataframe

boot(Portfolio, alpha.fn, R=1000) #returns alpha = 0.5758 and SE = 0.0886



#Estimating the accuracy of a Linear Regression Model (coefficients and predictions)
#Dataset: Auto (392 x 9)
#Goal: estimate accuracy of model's intercept and slope using HP to predict MPG.

boot.fn = function(data, index){
    return(coef(lm(mpg ~ horsepower, data = Auto, subset = index)))
}
boot.fn(Auto, 1:392) #calculate lm coefficients using all observations (39.936 and -0.158)

#Use boot.fn() repeatedly to produce bootstrap estimates for the coefficients
set.seed(1)
boot.fn(Auto, sample(392, 392, replace = T))
#returns 38.739 and -0.148
boot.fn(Auto, sample(392, 392, replace = T))
#returns 40.038 and -0.160

#Use built-in boot() function from ISLR library to compute SE froom 1000 boostrap estimates
#of the intercept and slope terms
boot(Auto, boot.fn, 1000) #returns SE 0.86 for intercept and 0.0074 for slope

#Comparing above results with SE estimates computed by lm() function
summary(lm(mpg ~ horsepower, data = Auto))$coef #returns SE 0.717 and 0.0064, respectively
#the bootstrap estimate is MORE ACCURATE because it doesn't rely on the assumptions used
#for calculation of SE in the linear model summary. The population (noise) variance is
#unknown, and calculated via RSS, which relies on the model being correct. We have 
#seen that MPG and HP have a non-linear relationship. In addition, the standard formulae
#for SE also assume that the predictors are fixed (not too realistic), and so that all the 
#variability comes from variation in the irreducible errors.

#Let's now compare the bootstrap SE estimates vs standard SE formulae for a quadratic fit
boot.fn = function(data, index){
    coefficients(lm(mpg ~ horsepower + I(horsepower ^ 2), data = data, subset = index))
}
set.seed(1)
boot(Auto, boot.fn, 1000) #returns SE 2.094, 0.033, 0.0001 respectively

summary(lm(mpg ~ horsepower + I(horsepower ^ 2), data = Auto))$coef
#returns 1.800, 0.031, 0.0001, respectively... as expected, much closer now!






