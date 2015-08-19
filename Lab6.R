#Lab Chapter 6: DIMENSION REDUCTION METHODS

#MODEL SELECTION SCRIPT

#This lab uses the Hitters (MLB) dataset (322 x 20) from the ISLR library
library(ISLR)
?Hitters
summary(Hitters)

#The goal is to determine best model to predict the salary of a MLB player.
#The Salary variable contains NA's. We choose to eliminate these rows from the data:
Hitters = na.omit(Hitters)
with(Hitters, sum(is.na(Salary))) #check if all NA's were removed (sum is zero if yes)

#BEST SUBSET SELECTION METHOD: package leaps.
library(leaps)
#Let's fit the full model ("." is a short hand for all predictor variables)
regfit.full = regsubsets(Salary ~ ., data = Hitters)
summary(regfit.full) #note that default is for only 8 variables per subset

#Let's increase the size of subsets to 19 variables (all predictors):
regfit.full = regsubsets(Salary ~ ., data = Hitters, nvmax = 19) #still super fast!
reg.summary = summary(regfit.full)
names(reg.summary) #show list of objects returned by the model

##Let's create plots for RSS, BIC, and adjusted R squared for each model:
par(mfrow = c(2,2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "b", col = "blue")

plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted Rsq", type = "b")
which.max(reg.summary$adjr2) #returns 11
points(11, reg.summary$adjr2[11], col = "red", cex = 1, pch = 20)

plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "b")
which.min(reg.summary$bic) #returns 6
points(6, reg.summary$bic[6], col = "red", pch = 20)

#Let's also plot the Cp for each model (different number of variables):
plot(reg.summary$cp, xlab = "Number of variables", ylab = "Cp")
which.min(reg.summary$cp) #R base function to query index of min value of an object
points(10, reg.summary$cp[10], pch = 20, col = "red") #x,y define position of point to color
reg.summary$cp[10] #returns 5.009317

#Let's use package plot method for the regsubsets object:
plot(regfit.full, scale = "Cp") #matrix showing all variables selected per Cp value
names(regfit.full)[10] #10 is the model id (in this case, id = p)
coef(regfit.full, 10) #returns all coefficients. Hits and Walks are the most influential

plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")

plot(regfit.full, scale = "bic")
coef(regfit.full, 6) #coefficients for model with minimum BIC

#FORWARD STEPWISE SELECTION
#regsubsets function must have method="forward" specified:
regfit.fwd = regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "forward")
regfwd.summary = summary(regfit.fwd)
plot(regfwd.summary$cp, xlab = "Number of variables", ylab = "Cp")
which.min(regfwd.summary$cp) #returns 10, like subset selection
plot(regfit.fwd, scale = "Cp")
names(coef(regfit.fwd, 10)) #it happens to select same model as subset method

#Let's also do backward selection to compare results:
regfit.bwd = regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "backward")
summary(regfit.bwd)

#comparing 7-variable models by best subset, forward and backward selection: all different!
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

#Instead of Mallow's Cp, let's use VALIDATION SET to select best model
#Let's split the data into ~ 68% training and 32% validation
dim(Hitters) #returns 263 x 20 (removed NA's)
set.seed(1)
train = sample(seq(263), 180, replace = F) #split must be random!
train

#Let's train the model
regfit.fwd = regsubsets(Salary ~ ., data = Hitters[train, ], nvmax = 19, method = "forward")

#Let's make predictions on the observations not used for training. There is no predict
#method for regsubsets, so we must hard code it. First, let's set up a vector to fill with
#the validation errors for each of the 19 models:
val.errors = rep(NA, 19)

#next, we create a matrix with the validation data set using the model.matrix() function:
x.test = model.matrix(Salary ~., data = Hitters[-train, ])
dim(x.test) #return 83 x 20

#now, let's calculate the predictions:
for(i in 1:19){
    coefi = coef(regfit.fwd, id = i)
    pred = x.test[ , names(coefi)] %*% coefi #matrix multiplication of predictors * coeff
    val.errors[i] = mean((Hitters$Salary[-train] - pred) ^ 2) #squared error difference
}

which.min(val.errors) #returns 5 (best forward stepwise model)

#Let's plot the validation and training error curves:
plot(sqrt(val.errors), ylab = "Root MSE", ylim = c(300, 400), pch = 19, type = "b")
points(sqrt(regfit.fwd$rss[-1]/180), col = "blue", pch = 19, type = "b") #[-1] remove intercept
legend("topright", legend = c("Training", "Validation"), col = c("blue", "black"), pch = 19)

#Let's write a prediction function to be used again later:
prediction.regsubsets = function(object, newdata, id, ...){
    form = as.formula(object$call[[2]]) #extract object model formula for y ~ x
    mat = model.matrix(form, newdata) #set prediction matrix
    coefi = coef(object, id = id) #obtain matrix of coefficients
    mat[, names(coefi)] %*% coefi #calculate predictions matrix
}

#Now let's try CROSS-VALIDATION (10-fold) to choose the best model:
#First, we create a vector with the length of the nobs of the dataset
set.seed(11) #the seed number will define the "random" combination
folds = sample(rep(1:10, length = nrow(Hitters))) #1:10 as this is the number of folds
folds
table(folds) #random breakdown of indexes for each fold

#Let's create a matrix to fill with the CV values per fold (10), per model (19):
cv.errors = matrix(NA, 10, 19)

for(k in 1:10){
    best.fit = regsubsets(Salary ~., data = Hitters[folds != k, ], nvmax = 19, 
                          method = "forward")
    for(i in 1:19){
        pred = prediction.regsubsets(best.fit, Hitters[folds == k, ], id = i)
        cv.errors[k, i] = mean((Hitters$Salary[folds == k] - pred) ^ 2)
    }
}

rmse.cv = sqrt(apply(cv.errors, 2, mean))
plot(rmse.cv, pch = 19, type = "b") #less noise in 10-fold CV compared to validation set


#RIDGE REGRESSION AND LASSO - glmnet package
#glmnet doesn't use the model formula language, so we need to set up x and y
library(glmnet)
x = model.matrix(Salary ~. -1, data = Hitters) #-1 ensures y is not included
y = Hitters$Salary

#Let's fit a ridge regression model (alpha = 0):
#fit.ridge = glmnet(x, y, alpha = 0) #glmnet standardize values by default!
#plot(fit.ridge, xvar = "lambda", label = TRUE) #plot method for glmnet

#Now, let's do cross-validation to choose the best lambda (nfolds = 10 by default):
#cv.ridge = cv.glmnet(x, y, alpha = 0)
#plot(cv.ridge) #shows dashed line at lowest MSE and 1 sigma from the lowest

#THIS SECOND PART IS IN THE BOOK BUT NOT IN THE VIDEO
#We can also specify the range of lambda values to be used in the fit:
grid = 10 ^ seq(10, -2, length = 100) #create range of values from 10^10 to 10^-2
ridge.mod = glmnet(x, y, alpha = 0, lambda = grid)
dim(coef(ridge.mod)) #returns 21 variables (incl intercept) by 100 models (one per lambda)

#Let's check the shrinkage for different values of lambda:
ridge.mod$lambda[50] #returns 11498 -> large lambda
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1, 50] ^ 2)) #returns 6.36, [-1] is to disregard intercept coef

ridge.mod$lambda[60] #returns 705 -> large lambda
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1, 60] ^ 2)) #returns 57.1

#Let's make predictions for a specific value of lambda:
?predict.glmnet
#s is lambda value (default to all), and the row range is for the variables:
predict(ridge.mod, s = 50, type = "coefficients")[1:21,]

#Let's now split the samples into a training and a test set to estimate the test error
set.seed(1)
train.new = sample(1:nrow(x), nrow(x) / 2) #splitting in two halves
test = (-train.new) #turn train index values negative
y.test = y[test] #all y values except those used in training the model

#Let's fit a ridge regression model on the training set, evaluate its MSE on the test set
#using lambda = 4.
ridge.mod = glmnet(x[train.new, ], y[train.new], alpha = 0, lambda = grid, thresh = 1e-12)
#why use threshold here? Force coefficients to shrink to zero?

#This time we use the newx arg instead of coefficients
ridge.pred = predict(ridge.mod, s = 4, newx = x[test, ])
mean((ridge.pred - y.test) ^ 2) #mean squared error, returns 101025

mean((mean(y[train.new]) - y.test) ^ 2) #MSE for null model, fit with just the intercept (193253)
#we get same result by fitting a ridge regression model with a very large lambda:
ridge.pred = predict(ridge.mod, s = 1e10, newx = x[test, ])
mean((ridge.pred - y.test) ^ 2) #returns 193253

#Let's now check whether there is any advantage to fit a ridge regression model with
#lambda = 4 instead of just performing least squares regression (lambda = 0)
ridge.pred = predict(ridge.mod, s = 0, newx = x[test, ], exact = T) 
#exact arg is used to yield exact least squares instead of an approximation (p.253)
mean((ridge.pred - y.test) ^ 2) #returns 114783
lm(y ~ x, subset = train.new)
predict(ridge.mod, s = 0, exact = T, type = "coefficients")[1:21, ]
#same results as lm() above, although intercept coefficient is offset.

#Let's then use CV to choose a value for lambda, instead of an ad-hoc value:
set.seed(1)
cv.out = cv.glmnet(x[train.new, ], y[train.new], alpha = 0) #CV using only the training set
plot(cv.out)
best.lambda = cv.out$lambda.min #returns the minimum, not the 1 sigma value from the min
best.lambda #returns 212

#Let's now check the test MSE for the model using the lowest lambda:
ridge.pred = predict(ridge.mod, s = best.lambda, newx = x[test, ])
mean((ridge.pred - y.test) ^ 2) #returns 96016, better than lm() results, or lambda = 4

#Finally, we refit our selected ridge regression model on the full data set using best lambda
out = glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = best.lambda)[1:21, ]
plot(out, xvar = "lambda", label = TRUE) #as expected, no coef is zero



#Let's now fit a LASSO regression model (alpha = 1):
fit.lasso = glmnet(x, y, alpha = 1)
plot(fit.lasso, xvar = "lambda", label = T) #note number of predictors (top) vary by model

#And proceed with CV to choose the best fit:
cv.lasso = cv.glmnet(x, y) #alpha is 1 by default. Values b/w 0 and 1 give elastic net.
plot(cv.lasso) #chosen model has 6 predictors instead of 20
coef(cv.lasso) #gives sparse matrix with coef for chosen model (1 sigma from lowest MSE)

#Let's instead use the validation set split of the data to choose lambda:
lasso.train = glmnet(x[train, ], y[train]) 
lasso.train #give degrees of freedom, %deviance, for each lambda (up to 100 values)
#note that it stopped at the 89th iteration as deviance was no longer changing

pred = predict(lasso.train, x[-train, ]) #test salary prediction on remaining 83 observations
dim(pred) #returns 83 x 89 (nobs x number of lambdas/models)
rmse = sqrt(apply((y[-train] - pred) ^ 2, 2, mean)) #calculate test RMSE
plot(log(lasso.train$lambda), rmse, type = "b", xlab = "Log(lambda)")

#if we order lambda values by MSE, we can determine the smallest (first in ordered sequence)
lambda.best = lasso.train$lambda[order(rmse)[1]] #returns 19.99
coef(lasso.train, s = lambda.best) #return coef for given lambda value (now p = 8 + intercept)
#the model defined by choosing lambda with validation set was different than that obtained
#by using 10-fold CV.

#THIS SECOND PART IS IN THE BOOK BUT NOT IN THE VIDEO
#Let's now use lasso again to fit a model on the training set defined in the second part of
#ridge regression exercise, for comparison of results:
lasso.mod = glmnet(x[train.new, ], y[train.new], alpha = 1, lambda = grid)
plot(lasso.mod) #train.new is the set defined in the book (line 173)

#let's now perform CV to choose lambda and compute the test MSE:
set.seed(1)
cv.out = cv.glmnet(x[train.new, ], y[train.new], alpha = 1)
plot(cv.out)

best.lambda = cv.out$lambda.min #another way of getting lowest lambda than that shown in video
lasso.pred = predict(lasso.mod, s = best.lambda, newx = x[test, ])
mean((lasso.pred - y.test) ^ 2) #returns 100743, higher but similar to ridge

out = glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef = predict(out, s = best.lambda, type = "coefficients")[1:21, ]
lasso.coef
lasso.coef[lasso.coef!= 0] #model with lowest lambda has only 8 predictors


#PRINCIPAL COMPONENTS REGRESSION - use pcr() function from pls library
library(pls)
set.seed(2)
#use scale = T to standardize all predictors, and "CV" for 10-fold cv for each possible 
#value of M (19), the number of principal components. Missing values must have been removed!
pcr.fit = pcr(Salary ~., data = Hitters, scale = T, validation = "CV")
summary(pcr.fit)

#note the CV error is the root mean square error, this must be squared to calculate the MSE:
names(pcr.fit)
names(pcr.fit$validation)
head(pcr.fit$validation$adj)

#we can plot the cross-validation scores using the validationplot() function:
validationplot(pcr.fit, val.type = "MSEP") #val.type = MSEP will plot the CV MSE
#summary shows the lowest CV error at M=16, which is almos like performing least squares,
#because when all components are used in PCR, no dimension reduction occurs. However,
#from the plot we also see that the CV error with just one component is roughly the same,
#suggesting that a model that uses a small number of components may suffice.

#let's now perform PCR in the training data and evaluate its test set performance:
set.seed(1)
pcr.fit = pcr(Salary ~., data = Hitters, scale = T, validation = "CV", subset = train.new)
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP") #now the lowest CV error is for M = 7
#pcr.pred = predict(pcr.fit, x[-train.new, ], ncomp = 7) #BUG WITH x ncol should exclude X0?
#mean((pcr.pred - y.test) ^ 2) #should return 96556, equivalent to ridge and lasso

#finally, we refit using the full data set with the chosen M
pcr.fit = pcr(y ~ x, scale = T, ncomp = 7) #results slightly different from book
summary(pcr.fit)


#PARTIAL LEAST SQUARES - use plsr() function from pls library
set.seed(1)
pls.fit = plsr(Salary ~., data = Hitters, subset = train.new, scale = T, validation = "CV")
summary(pls.fit) #lowest CV errors when M = 2
#pls.pred = predict(pls.fit, x[-train.new, ], ncomp = 2) #same BUG as above
#mean((pls.pred - y.test) ^ 2) #should return 101417, comparable but a bit higher than others

#now we refit using the full data set with the chosen M:
pls.fit = plsr(Salary ~., data = Hitters, scale = T, ncomp = 2)
summary(pls.fit)
#note that the percentage variance in Salary that the 2-component PLS fit explains, 46.4%,
#is almost as much as that explained by using the 7-component PCR model fit, 46.49%. This
#is because PCR only attempts to maximize the amount of variance explained in the
#predictors, while PLS searches for directions that explain variance in both the predictors
#and the response.






