#Lab Chapter 8: Trees

#data(package = "MASS")$results #shows all datasets in the specified package
#data(package = "ISLR")$results[,3:4] #same

#Classification and Regression Trees (CART) - package tree
require(ISLR)
require(tree)

#Classification Trees
attach(Carseats) #dataset to be used, Sales (binary representation) will be response variable
dim(Carseats) #400 x 11
names(Carseats)
hist(Sales) #check distribution and average value for Sales (~ 8 in thousands)
High = ifelse(Sales <= 8, "No", "Yes") #create binary sales variable (MUST be factor type)
Carseats = data.frame(Carseats, High) #add new variable to Carseats dataframe

#Let's fit a tree to the dataset, excluding the response variable, and plot the result
tree.carseats = tree(High ~ . - Sales, data = Carseats)
summary(tree.carseats) #bushy tree with 27 terminal nodes, used 8 variables
plot(tree.carseats)
text(tree.carseats, pretty = 0) 
#pretty=0 instructs R to include category names for any
#qualitative predictor, rather than simply displaying a letter for each category.
tree.carseats 
#calling tree will print a detailed summary: splt criterion, nobs in that
#branch, deviance, overall prediction for the branch (Y/N), and fraction of obs for that
#branch that takes on values Yes and No.

#Let's now create a training ans test set (250, 150) split of the 400 obs, grow the tree
#in the training set, and evaluate its performance on the test set
set.seed(1011) #always good to change/vary seed!
train = sample(1:nrow(Carseats), 250)
tree.carseats = tree(High ~ . - Sales, data = Carseats, subset = train)
plot(tree.carseats) #training tree has 20 terminal nodes and 7 variables
text(tree.carseats)

tree.pred = predict(tree.carseats, Carseats[-train, ], type = "class") 
#type "class" returns actual class prediction; another type would be probability, more 
#details at ?predict.tree
with(Carseats[-train, ], table(tree.pred, High)) #confusion table for predictions on test data
#from table: model accuracy = 72 + 33 / 150 = 70%

#Let's use CV to prune the tree, since it has grown to full depth and might be too variable
cv.carseats = cv.tree(tree.carseats, FUN = prune.misclass)
#FUN=prune.misclass instructs that we want the classification error rate to guide the
#cross-validation and pruning process, rather than the (regression) default which is deviance.
names(cv.carseats) #k is the cost complexity parameter; despite the name, dev here
#corresponds to the cross-validation error rate.
cv.carseats #trees with 13 terminal nodes has the lowest CV error.
plot(cv.carseats) #not smooth due to small nobs in dataset. Tree size 13 seems a good choice.
par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = 'b')
plot(cv.carseats$k, cv.carseats$dev, type = 'b')

prune.carseats = prune.misclass(tree.carseats, best = 13) 
#note that pruning is done in the FULL data set!
plot(prune.carseats)
text(prune.carseats, pretty = 0)

#Now, let's evaluate the pruned treew on the test data
tree.pred = predict(prune.carseats, Carseats[-train, ], type = "class")
with(Carseats[-train, ], table(tree.pred, High))
#from table: model accuracy = 72 + 32 / 150 = 69.3%
#no accuracy improvement, but better interpretability

#Regression Trees
require(MASS) #package with dataset (Boston) to be used
dim(Boston) #506 x 14
names(Boston) #response variable will be medv, median house value in 1K dollars

#let's fit a tree in the trainign data
set.seed(1)
train = sample(1:nrow(Boston), 300) #split train:test in 300:206
tree.boston = tree(medv ~., Boston, subset = train)
summary(tree.boston) #shows only 3 variables used to grow tree. Deviance is the SSE for tree.
plot(tree.boston)
text(tree.boston, pretty = 0)
#example of result from tree: median value of $45980 for large homes in suburbs with high
#socio-economic status (rm >= 7.437 and lstat < 9.175)

#let's now use CV to check if pruning increases performance
cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = 'b') #size 8 is best

#In case we want to prune the tree (not expected to improve results in this case):
prune.boston = prune.tree(tree.boston, best = 5) #arbitrary choie of best size
plot(prune.boston)
text(prune.boston)

#In keeping with the CV results, we use the unpruned tree to make predictions on test set
yhat = predict(tree.boston, newdata = Boston[-train, ])
boston.test = Boston[-train, 'medv']
plot(yhat, boston.test)
abline(0, 1)
mean((yhat - boston.test) ^ 2) #test set MSE associated with the regression tree is 20.94,
#thus square root is about 4.58 (sd), indicating that this model leads to estimates within
#around $4580 of the true median home value for the suburb.


#RANDOM FORESTS - randomForest package
require(randomForest)

#Bagging is a special case of RF with m = p (p = 13 for Boston data set)
set.seed(1)
bag.boston = randomForest(medv ~., data = Boston, subset = train, mtry = 13, importance = T)
bag.boston

#Let's check how well this model perform on the test set
yhat.bag = predict(bag.boston, newdata = Boston[-train, ]) #use ntree to change # of trees
plot(yhat.bag, boston.test)
abline(0, 1)
mean((yhat.bag - boston.test) ^ 2) #already much lower than CART! (13.02)


#Let's now fit a random forest
set.seed(101)
rf.boston = randomForest(medv ~., data = Boston, subset = train)
#note: by default, m = p/3 for regression trees, and sqrt(p) for classification.
rf.boston #automatic choice of 500 trees, mtry=4; MSR and %variance are based on OOB estimates
importance(rf.boston) #print importance of each predictor (not ordered)
varImpPlot(rf.boston) #plot importance measures

#Let's now plot the error for all possible mtry values (13) to choose mtry ourselves
oob.err = double(13) #create vector with 13 numeric zeros
test.err = double(13) #double precision vector is a floating point format with 64 bits
for(mtry in 1:13){
    fit = randomForest(medv ~., data = Boston, subset = train, mtry = mtry, ntree = 400)
    oob.err[mtry] = fit$mse[400]
    pred = predict(fit, Boston[-train, ])
    test.err[mtry] = with(Boston[-train, ], mean((medv - pred) ^ 2))
    cat(mtry, " ") #will value after each iteration, concatenated with single space
}
#note: cat is useful in case loop runs slowly; ntree = 400 selected arbitrarily

test.err[which.min(test.err)] #14.42 (mtry = 5)
oob.err[which.min(oob.err)] #12.49 (mtry = 4) #(automatic) slight improvement over bagging

matplot(1:mtry, cbind(test.err, oob.err), pch = 19, col = c("red", "blue"), type = "b", 
        ylab = "Mean Squared Error")
legend("topright", legend = c("OOB", "Test"), pch = 19, col = c("red", "blue"))
#note that mtry = 1 is equivalent to CART, and mtry = 13 is equivalent to bagging
#we'd expect the curves to follow each other and not have much difference if the nobs is 
#high enough (thus lower standard error)


#BOOSTING - gbm package
require(gbm)
#Boosting builds lots of smaller trees. Unlike random forests, each new tree in boosting
#tries to patch up the deficiencies of the current ensemble. We use gaussian disriibution
#for regression trees, and bernoulli for binary classification problems. Default for
#lambda (shrinkage) is 0.001.
boost.boston = gbm(medv ~., data = Boston[train, ], distribution = "gaussian",
                   n.trees = 10000, shrinkage = 0.01, interaction.depth = 4)
summary(boost.boston) #lstat and rm are the most influent variables
#let's plot partial dependency plots for most important variables to illustrate their
#marginal effect on the response after integrating out the other variables
plot(boost.boston, i = "lstat") #median house prices decrease with lstat
plot(boost.boston, i = "rm") #median house prices increase with rm

#Let's make a prediction on the test set (we should use CV to select the number of trees and 
#prevent overfitting), and compute the test error as a function of the number of trees.
n.trees = seq(from = 100, to = 10000, by = 100) #100 values
predmat = predict(boost.boston, newdata = Boston[-train, ], n.trees = n.trees)
dim(predmat) #computed results as a matrix (206 x 100) without iterating

berr = with(Boston[-train, ], apply((predmat - medv) ^ 2, 2, mean))
#apply computes column-wise MSE; vector medv is recycled to calculate error matrix
plot(n.trees, berr, pch = 19, ylab = "Mean Squared Error", xlab = "Number of Trees",
     main = "Boosting Test Error")
abline(h = min(test.err), col = "red") #add horizontal line with min RF test error
#Note that boosting results in lower MSE than RF.

















