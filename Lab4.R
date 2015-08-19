#Lab Chapter 4: CLASSIFICATION METHODS

#options(digits = 3) #sets number of printed decimals to 3 (default = 7)
#library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket[,-9]) #correlation matrix of all variable pairs, except non-numerical Direction (9)
attach(Smarket)
plot(Volume, pch=20)

**********************************************************************************************
#LOGISTIC REGRESSION (GLM)
#similar to lm, use family = binomial for logistic regression
#Model to predict Direction based on Lag1 to 5 and Volume
glm.fit = glm(formula = Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
              data = Smarket, family = binomial)
summary(glm.fit)
#Lag1 shows the smallest p-value, and the negative coefficient suggests if the market had a
#positive return yesterday, it is less likely to go up today. However, p-value is still too
#high for clear evidence of an association with the response variable.

coef(glm.fit) #print coefficents
summary(glm.fit)$coef #print top summary table
summary(glm.fit)$coef[,4] #print p-values from table

glm.probs = predict(glm.fit, type = "response") #type = "response" outputs P(Y=1|X)
glm.probs[1:10] #print first 10 predicted probabilities for first 10 predictor values
contrasts(Direction)

#convert predicted probabilities into class labels
glm.pred = rep("Down", 1250) #create vector with 1250 values of "Down"
glm.pred[glm.probs > 0.5] = "Up" #assign "Up" to glm.pred for all prob > 50% in glm.prob

#create a confusion matrix to determine how many observations were correctly classified
table(glm.pred, Direction)
#calculating the training error rate*
mean(glm.pred == Direction) #same as (507+145)/1250, all correctly classified predictions
#*it's the training error rate because we used the same 1250 data to train and test the model

#obtaining a more realistic test error rate by leaving out all 2005 data, and training on the
#data from 2001 to 2004.
train = (Year < 2005) #boolean vector with all 1250 elements, set to TRUE if before 2005
Smarket.2005 = Smarket[!train,] #includes only 2005 data
dim(Smarket.2005) #returns 252 by 9
Direction.2005 = Direction[!train]
summary(Direction.2005) #down = 111, up = 141

#Fit (train) logistic regression model only on data before 2005 (using subset argument)
glm.fit = glm(formula = Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
              data = Smarket, family = binomial, subset = train)

#Calculate probabilities in the test data (Smarket.2005)
glm.probs = predict(glm.fit, Smarket.2005, type = "response")

#Create confusion table to access model performance
glm.pred = rep("Down", 252)
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction.2005) #compare model prediction with actual values

#determine the test set (year 2005) error, i.e. the values that do not match Direction.2005
mean(glm.pred != Direction.2005) #returns 0.52 MAKE SURE TO RUN ALL LINES OF CODE!

#Re-fit model with only Lag1 and Lag2 to try to improve performance
glm.fit = glm(formula = Direction ~ Lag1 + Lag2, data = Smarket, family = binomial, 
              subset = train)
glm.probs = predict(glm.fit, Smarket.2005, type = "response")
glm.pred = rep("Down", 252)
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005) #test "correctness" rate, returns 0.56, no better than naive
up_accuracy = 106/(106+76) #return 0.582

#Use re-fitted model to predict returns for specific lag1 and lag2 values
predict(glm.fit, newdata = data.frame(Lag1 = c(1.2, 1.5), Lag2 = c(1.1, -0.8)), 
        type = "response") #return 0.479 for 1st combination, and 0.496 for the next day

*********************************************************************************************
#LINEAR DISCRIMINANT ANALYSIS
#Use lda() from MASS lib with similar syntax than lm and glm functions, but no "family" arg
lda.fit = lda(formula = Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.fit
plot(lda.fit)
#output show prior prob for Down = 49.2% (i.e. 49.2% of the training observations 
#correspond to days that the market went down), and 50.8% for Up. The group means shown in the 
#output are the average of each predictor within each class, and here they suggest a tendency
#for the previous 2 days' returns to be negative on days when the market increases, and 
#to be positive when the market declines. Finally, the coefficients of linear discriminants
#output provide linear combination of Lag1 and Lag2 that are used as multipliers of the 
#elements of X = x in the decision rule. The plot shows these linear discriminants for "Down"
# and "Up", by computing coef1 x Lag1 + coef2 x Lag2.

#Make predictions based on the lda model
lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred) #returns "class" (up or down), the "posterior" probability (for up and down),
                #and "x" (the combined linear discriminants)
#Create confusion matrix
lda.class = lda.pred$class
table(lda.class, Direction.2005) #results similar to logistic regression
mean(lda.class == Direction.2005) #returns 0.56, "correctness" rate

#Re-create posterior probabilities by applying 50% threshold
sum(lda.pred$posterior[,1] >= 0.5) #returns 70
sum(lda.pred$posterior[,1] < 0.5) #returns 182 (market decrease)
head(lda.pred$posterior)
lda.pred$posterior[1:20, 1] #print 20 posterior probs for "Down" (column 1)
lda.class[1:20] #print labels of 20 predictions (posterior prob when market will go down)

#Change threshold for posterior probability (when market will decrease) to 90%
sum(lda.pred$posterior[,1] > 0.9) #prob of 90% that market will decrease (0 for t > 52%)

*********************************************************************************************
#QUADRATIC DISCRIMINANT ANALYSIS
#Use qda() function from MASS library
qda.fit = qda(formula = Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
qda.fit #no output for linear discriminants since it's a quadratic function

#Predict class (for 2005 data) based on QDA model
qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005) #compare to actual values
mean(qda.class == Direction.2005) #returns 0.6, which is better accuracy than previous
#models, indicating quadratic component may better reflect true relationship, but it's
#necessary to test model in a larger set to be sure.

*********************************************************************************************
#K-NEAREST NEIGHBORS CLASSIFIER
#Use knn() function from the class library. It requires four inputs: 1) a matrix containing 
#the predictors associated with the training data; 2) a matrix containing the predictors 
#associated with the test data; 3) a vector containing the class labels for the training
#observations; 4) a value for K, the number of nearest neighbors to be used to classify.
library(class)
train.X = cbind(Lag1, Lag2)[train, ] #1)
test.X = cbind(Lag1, Lag2)[!train, ] #2)
train.Direction = Direction[train] #3)

#note: KNN doesn't fit the model, then use the model for prediction. The prediction is 
#achieved in one step. This is then to predict the market movement for dates in 2005:
set.seed(1) #if several obs are tied as nearest neighbors, R will randomly break the ties
knn.pred = knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005) #returns 50%

#let's try to improve the results by increasing k
knn.pred = knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005) #returns 53.6%
#model doesn't improve beyond 53.6% for k>3. It seems QDA was a better fit for this data.

*********************************************************************************************
#ANOTHER APPLICATION: CARAVAN INSURANCE DATA
#Data: 85 predictors with demographic characteristics for 5822 individuals
#Response variable: Purchase (whether or not a given individual purchases caravan insurance)

#library(ISLR)
dim(Caravan) #returns 5822 by 86
attach(Caravan)
summary(Purchase) # ~6% of individuals in dataset have purchased the insurance

#Classification using KNN
#IMPORTANT: scale of the predictors matter a lot in KNN, due to the effect on the distance.
#Thus, larger scales have much stronger effect on the results. Consequently, it's 
#necessary to standardize the data to be on the same scale, with mean=0 and sd = 1.

standardized.X = scale(Caravan[,-86]) #column 86 is excluded b/c it's the response variable.

#Demonstrating the results of standardization for 2 columns of data:
var(Caravan[,1]) #returns 165
var(Caravan[,10]) #returns 3.65
var(standardized.X[,1]) #returns 1
mean(standardized.X[,1]) #returns 0
var(standardized.X[,10]) #returns 1

#Split data into test set with first 1000 observations, fit model on training data with k=1
test = 1:1000
train.X = standardized.X[-test,]
test.X = standardized.X[test, ]
train.Y = Purchase[-test]
test.Y = Purchase[test]

set.seed(1)
knn.pred = knn(train.X, test.X, train.Y, k = 1)
mean(test.Y != knn.pred) #returns 0.118, error rate seems good, but not really: error rate
#would be only 6% by always predicting "No" regardless of predictors!
mean(test.Y != "No") #returns 0.059, confirming the above statement.

#For this application however, the test error rate is less important than the correctly
#predicted fraction of individuals interested in buying the insurance.
table(knn.pred, test.Y) # 9 out of 77 individuals were correctly predicted to buy insurance.
#This is 11.7% or almost twice as best than random guessing (6% prior).

#Testing model for k=3 and k=5
knn.pred = knn(train.X, test.X, train.Y, k = 3)
table(knn.pred, test.Y) #19.2% accuracy (5 out of 26)

knn.pred = knn(train.X, test.X, train.Y, k = 5) #best result
table(knn.pred, test.Y) #26.7% accuracy for Yes (4 out of 15)
mean(test.Y != knn.pred)

#CLASSIFICATION USING LOGISTIC REGRESSION
glm.fit = glm(Purchase ~., data = Caravan, family = binomial, subset = -test)
glm.probs = predict(glm.fit, Caravan[test, ], type = "response")
glm.pred = rep("No", 1000)
glm.pred[glm.probs > 0.5] = "Yes"
table(glm.pred, test.Y) #yields zero "Yes" correct prediction

glm.pred = rep("No", 1000) #reset glm.pred
glm.pred[glm.probs > 0.25] = "Yes" #adjust to predict a purchase anytime prob > 25%
table(glm.pred, test.Y) #33.3% "Yes" accurate prediction: better than KNN or random guess
mean(test.Y == glm.pred) #(919+11)/1000 => 93% model accuracy (correctly predicts Yes and No)




