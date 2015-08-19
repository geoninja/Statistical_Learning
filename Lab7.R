#Lab Chapter 7: Non Linear Models

#require(ISLR)
attach(Wage) #we will use the Wage data set

#POLYNOMIALS
#using a single predictor (Age)

#let's use the poly function, which generates a basis of orthogonal polynomials
fit = lm(wage ~ poly(age, 4), data = Wage) #degree d = 4
summary(fit) #using orthogonal basis (for linear regression, single predictor) we can
             #look at the p-values and conclude the quartic term is not relevant. We can
             #do the same analysis in other cases using ANOVA

#let's make a plot of the fit and their SE
age.lim = range(age)
age.grid = seq(from = age.lim[1], to = age.lim[2]) #create a sequence to test model prediction
preds = predict(fit, newdata = list(age = age.grid), se = T)
se.bands = cbind(preds$fit + 2 * preds$se, preds$fit - 2 * preds$se) #create conf intervals
plot(age, wage, col = "darkgrey") #plot data
title("Degree 4 Polynomial", outer = T) #create a figure title that spans both subplots
lines(age.grid, preds$fit, lwd = 2, col = "blue") #add fit
matlines(age.grid, se.bands, col = "blue", lty = 2) #add SE bands (matrix)

#alternatively, we could fit he polynomial function this way:
fitA1 = lm(wage ~ age + I(age ^ 2) + I(age ^ 3) + I(age ^ 4), data = Wage) #not orthogonal base
summary(fitA) #note the coefficients are very different!
plot(fitted(fit), fitted(fitA1)) #however, the fit is the same
predsA1 = predict(fitA1, newdata = list(age = age.grid), se = T)
max(abs(preds$fit - predsA1$fit)) #another way of checking whether the fits are the same

#this would produce exactly the same fit as fitA above:
fitA2 = lm(wage ~ poly(age, 4, raw = T), data = Wage) #raw = T doesn't do orthogonalization
coef(summary(fitA2)) #same fit and coefficients as fitA

#this would also produce same model and fit as fitA:
fitA3 = lm(wage ~ cbind(age, age ^ 2, age ^ 3, age ^ 4), data = Wage)
#note: any function call, such as cbind, inside the formula also serves as a wrapper
summary(fitA3)

#using ANOVA to compare results of different models
#first, we create *nested* models in sequence, ie. the next model contains the previous
fita = lm(wage ~ education, data = Wage)
fitb = lm(wage ~ education + age, data = Wage)
fitc = lm(wage ~ education + poly(age, 2), data = Wage)
fitd = lm(wage ~ education + poly(age, 3), data = Wage)
anova(fita, fitb, fitc, fitd)

#let's now perform polynomial logistic regression
#binary response on wage: big earners = wage > 250
#we use the wrapper for the response so that instead of logical values, we get 0 and 1 values.
fit = glm(I(wage > 250) ~ poly(age, 3), data = Wage, family = binomial)
summary(fit)

#let's now test the model on the age.grid predictors
preds = predict(fit, list(age = age.grid), se = T)

#this time, let's create a matrix containing the SE's and respective fitted values
#to be used later to calculate the probabilities
se.bands = preds$fit + cbind(fit = 0, lower = -2 * preds$se, upper = 2 * preds$se)
se.bands[1:5, ] #just check first 5 rows

#since computations were done in the inverse logit scale, we need to transform it back by
#applying inverse logit mapping
prob.bands = exp(se.bands) / (1 + exp(se.bands)) 
matplot(age.grid, prob.bands, col = "blue", lwd = c(2, 1, 1), lty = c(1, 2, 2), type = "l",
        ylim = c(0, 0.1))
points(jitter(age), I(wage > 250) / 10, pch = "|", cex = 0.5) #jitter generates noise data
#note: wage is split in 10 because of ylim is at 0.1 (SE is very large after that, so this
#was done to adjust the plot for better visualization)

#Step functions
#we use the cut() function to fit a step function
table(cut(age, 4)) #create table showing chosen cut points
#cut() returns ordered categorical variable
fit = lm(wage ~ cut(age, 4), data = Wage) #lm() create set of dummy variables for cutpoints
#note: we can specify the cutpoints directly using the breaks option
coef(summary(fit)) #age values chosen were 33.5, 49, and 64.5 years
#note: age < 33.5 was left out, so the intercept coeffiecient of $94160 can be interpreted as
#the average salary for those under 33.5 years of age, and the other coefficients are the 
#average additional salaries for those in other age groups. We can produce predictions and
#plots in the same way as we did for the polynomial fit.


#SPLINES
#splines are more flexible than polynomials. Let's explore cubic splines using the splines lib
require(splines)
fit = lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage) #bs stands for basis
plot(age, wage, col = "darkgrey")
lines(age.grid, predict(fit, list(age = age.grid)), col = "darkgreen", lwd = 2)
#automatically create lines for fitted values
abline(v = c(25, 40, 60), lty = 2, col = "darkgreen")

dim(bs(age, knots = c(25, 40, 60))) #3000 x 6 (nobs x predictors)
#a cubic spline with 3 knots has 7 df: 1 intercept and 6 basis functions
dim(bs(age, df = 6)) #3000 x 6; use df option to produce knots at uniform quantiles
attr(bs(age, df = 6), "knots") #shows chosen knots at respective quantiles

#we use the ns() function to fit a natural spline
fit2 = lm(wage ~ ns(age, df = 4), data = Wage)
pred2 = predict(fit2, newdata = list(age = age.grid), se = T)
lines(age.grid, pred2$fit, col = "red", lwd = 2)

#the smoothing splines doesn't require a knot selection, but it does have a smoothing
#parameter specified via the effective degrees of freedom df
plot(age, wage, xlim = age.lim, cex = 0.5, col = "darkgrey")
title("Smoothing Spline")
fit = smooth.spline(age, wage, df = 16)
lines(fit, col = "red", lwd = 2)

#instead of specifying df, we can use LOOCV to select the parameter automatically
fit2 = smooth.spline(age, wage, cv = T)
fit2 #provides information about the fit, including chosen smoothing parameter (df = 6.79)
lines(fit2, col = "purple", lwd = 2)


#LOCAL REGRESSION - using loess() function
#note: locfit library may also be used for fitting local regression models
plot(age, wage, xlim = age.lim, cex = 0.5, col = "darkgrey")
title("Local Regression")
fit20 = loess(wage ~ age, span = 0.2, data = Wage)
fit50 = loess(wage ~ age, span = 0.5, data = Wage)
#the larger the span (% of obs for each neighborhood), the smoother the fit:
lines(age.grid, predict(fit20, data.frame(age = age.grid)), col = "red", lwd = 2)
lines(age.grid, predict(fit50, data.frame(age = age.grid)), col = "blue", lwd = 2)
legend("topright", legend = c("Span = 0.2", "Span = 0.5"), col = c("red", "blue"), 
       lty = 1, lwd = 2, cex = 0.8)


#GENERALIZED ADDITIVE MODELS
#use gam package to fit multiple non linear terms, includes plot and SE functions
require(gam)
gam1 = gam(wage ~ s(age, df = 4) + s(year, df = 4) + education, data = Wage)
#note: s stands for smoothing term
par(mfrow = c (1, 3))
plot(gam1, se = T) #plot model for each predictor including SE
summary(gam1) #p-values for year and age correspond to null hypothesis of linear relationship
#versus the alternative hypothesis of non-linear relationship.

#let's now create a gam for a binary response variable
gam2 = gam(I(wage > 250) ~ s(age, df = 4) + s(year, df = 4) + education, data = Wage, 
           family = binomial)
plot(gam2) #the plot for year looks linear...
summary(gam2)

#let's check, for example, if we need non linear terms for year
gam2a = gam(I(wage > 250) ~ s(age, df = 4) + year + education, data = Wage, 
           family = binomial)
anova(gam2a, gam2, test = "Chisq") #use chi-square for qualitative response
#conclusion: no, we don't need to add linear terms for year.

table(education, I(wage > 250)) #also, we see there are no high earners with less than HS
#let's refit model gam2a excluding the <HS category from education:
gam2b= gam(I(wage > 250) ~ s(age, df = 4) + year + education, family = binomial, 
           data = Wage, subset = (education != "1. < HS Grad"))
plot(gam2b, se = T, col = "green")

#gam's plot function works well even for models fit using lm or glm
lm1 = lm(wage ~ ns(age, df = 4) + ns(year, df = 4) + education, data = Wage)
#note: ns stands for natural spline, which is cubic by default
plot.gam(lm1, se = T) #use plot.gam() if fit was not done using gam

#let's make predictions from gam objects
gam3 = gam(wage ~ s(age, df = 4) + year + education, data = Wage)
preds = predict(gam3, newdata = Wage)

#let's now use local regression fits as building blocks in a GAM, use lo() function
gam.local = gam(wage ~ s(year, df = 4) + lo(age, span = 0.7) + education, data = Wage)
plot.gam(gam.local, se = T, col = "green")

#we can also use lo() to introduce interaction terms (between year and age):
gam.inter = gam(wage ~ lo(year, age, span = 0.5) + education, data = Wage)

#let's plot the resuting 2D surface using the akima package
library(akima)
plot(gam.inter) #one plot for each variable



