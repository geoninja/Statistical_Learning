#Lab Chapter 3 LINEAR REGRESSION

#library(ISLR)
library(MASS)
names(Boston)
View(Boston) #equivalent to "fix" function

#SIMPLE LINEAR REGRESSION where medv as response (y), and lstat as predictor (x)
lm.fit = lm(medv ~ lstat, data = Boston) #lm.fit is just a name
attach(Boston) #from here, no longer need to state data = Boston
summary(lm.fit)
names(lm.fit)
coef(lm.fit) #same as lm.fit$coefficients, return the regression coef.
confint(lm.fit) #return lower and upper confidence interval, level set at default 95%
confint(lm.fit, level = 0.9)
#using predict() for confidence intervals and prediction intervals for the prediction of y
#(medv) given a value of x (lstat)
predict(lm.fit, data.frame(lstat = c(5,10,15)), interval = "confidence")
predict(lm.fit, data.frame(lstat = c(5,10,15)), interval = "prediction")

plot(lstat, medv)
abline(lm.fit, col = 2, lwd = 3) #note: abline(a,b) will draw line with intercept a and slope b
plot(lstat, medv, pch = 20)
plot(lstat, medv, pch = "+")
plot(1:20, 1:20, pch = 1:20) #show all 20 pch types

par(mfrow = c(2,2))
plot(lm.fit) #produces 4 diagnostic plots
#alternatively, residuals can be plotted using residuals()
plot(predict(lm.fit), residuals(lm.fit)) #plot of fitted values vs residuals
plot(predict(lm.fit), rstudent(lm.fit)) #rstudent is studantized residuals
#alternative to plot leverage statistics
plot(hatvalues(lm.fit))
#index of value with highest leverage statistic
which.max(hatvalues(lm.fit)) #which.max identifies index of largest element of a vector


#MULTIPLE LINEAR REGRESSION
#syntax: lm(y~x1+x2+x3) for 3 predictors
lm.fit = lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)

lm.fit = lm(medv ~., data = Boston) #the dot means "use all predictors"
summary(lm.fit)
?summary.lm #show name of components of summary object
#examples:
summary(lm.fit)$r.sq
summary(lm.fit)$sigma #RSE residual squared error

library(car) #companion to applied regression package
vif(lm.fit) #variance inflation factor, a measure of the amount of multicollinearity, i.e.
#how much a variable is contributing to the SE of a regression. When multicollinearity is
#significant, this factor will be very high. Collinear variables are usually combined or
#eliminated. Multicollinearity can increase the variance of the regression coefficients,
#making them difficult to interpret. VIF=1: not correlated, 1-5: moderately correlated,
#5-10: highly correlated. Both tax and rad variables are strongly correlated.

lm.fit = lm(medv ~. -age, data = Boston) #uses all predictors except "age"
summary(lm.fit)
#alternatively, the update() function can be used
lm.fit1 = update(lm.fit, ~. -age)


#INTERACTION TERMS
#Examples: lstat:black includes an interaction between lstat and black
#lstat * black simultaneously include lstat, black, and the interaction lstat x black
#short hand for lstat + black + lstat:black

summary(lm(medv ~ lstat*age, data = Boston))

#NON-LINEAR TRANSFORMATIONS
#create a predictor x squared using I(x ^ 2). The function I() is needed to inhibit the 
#interpretation of operators as formula operators, so they're used as arithmetic operators.
lm.fit2 = lm(medv ~ lstat + I(lstat ^ 2))
summary(lm.fit2)

#let's use anova() to further quantify whether the quadratic fit is superior to the linear fit
lm.fit = lm(medv ~ lstat)
plot(medv ~ lstat)
abline(lm.fit, col = 2)
anova(lm.fit, lm.fit2) #performs hypothesis test comparing both models. Null hypothesis: both
#models fit equality well; alternative hypothesis: full model is superior. Since F statistic
#is high (135) and p-value is near zero, there is convincing evidence that full model is
#superior. This is ot surprising given previous evidence of non-linearity in the data.
par(mfrow = c(2,2))
plot(lm.fit) #before
plot(lm.fit2) #after transformation

#for polynomial fit, alternatively instead of using I():
lm.fit5 = lm(medv ~ poly(lstat, 5)) #similar to I(lstat ^ 5)
summary(lm.fit5) #results indicate a 5th order polynomial fit improves model

#log transformation
summary(lm(medv ~ log(rm), data = Boston)) #rm is number of rooms (average)


#QUALITATIVE PREDICTORS
View(Carseats)
names(Carseats)
lm.fit = lm(Sales ~. +Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)
attach(Carseats)
contrasts(ShelveLoc) #return code for dummy variables
#note: output for dummy variables ShelveLocGood and ShelveLocMedium have positive
#coefficient outputs, indicating they lead to higher sales, relative to the reference
#variable ShelveLoc = bad. Good location affects sales quite more than medium location.


#WRITING FUNCTIONS
LoadLibraries = function(){
    library(ISLR)
    library(MASS)
    print("The libraries have been loaded")
}
LoadLibraries()





