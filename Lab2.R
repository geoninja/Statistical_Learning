#Lab Chapter 2 - INTRODUCTION TO R (p.42-51)

#install.packages("ISLR")
#library(ISLR) #load library with datasets for course
#warning: ISLR was built under R version 3.1.2 (currently on 3.1.0)
#version #check current installed version of R

#reminder: both <- and = are assignment operators, but = has lower precedence and 
#it can only be used in the global environment. Be careful with use of no spaces with <-!

#Summing vectors (must have same lenght):

x <- c(1, 6, 2)
y <- c(1, 4, 3)
x + y #returns (2, 10, 5)

#Check all objects in the environment:
ls()
rm(x, y) #remove specified objects
#use rm(list = ls()) to remove all objects at once

#matrices
x <- matrix(data= c(1,2,3,4), nrow=2, ncol=2)
matrix(c(1,2,3,4), 2, 2, byrow=TRUE)
#element-wise matrix operations
sqrt(x)
x^2

#random normal distribution
x = rnorm(50) #n = 50, mean = 0, sd = 1
y = x + rnorm(50, mean = 50, sd = 0.1)
cor(x, y)

#use set.seed() to reproduce random numbers
set.seed(10) #arbitrary integer argument
x = rnorm(50)

#other stat functions
set.seed(1)
y = rnorm(100)
mean(y)
var(y)
sqrt(var(y)) # same as sd
sd(y)

#plotting
x = rnorm(100)
y = rnorm(100)
plot(x,y, xlab = "this is x-axis", ylab= "this is y-axis", main = "Plot of x and y")

#manually exporting a graph
pdf("Figure.pdf") #use function jpeg() to save in this format
plot(x, y, col= "green")
dev.off() # indicates we're done creating the plot

#using seq()
x = seq(1,10) #sequence of 1 to 10
x = 1:10 #same thing
x = seq (-pi, pi, length = 50)

#creating countour maps
#args: vector x, vector y, matrix containing z dimension for each (x,y) pair
y = x
f = outer(x, y, function(x, y) cos(y)/(1 + x ^ 2))
contour(x, y, f)
contour(x, y, f, nlevels = 45, add = T)
fa = (f - t(f)) / 2
contour(x, y, fa, nlevels = 15)

#image() is similar to contour() and produces a heatmap based on z coords
#persp() produces a 3D plot
image(x, y, fa)
persp(x, y, fa)
persp(x, y, fa, theta = 30) #theta is azimutal direction
persp(x, y, fa, theta = 30, phi = 20) #phi is colatitude angle
persp(x, y, fa, theta = 30, phi = 70)
persp(x, y, fa, theta = 30, phi = 40, col = "red", shade = 0.5, box = F)

#indexing
A = matrix(1:16, nrow=4)
A
A[2,3] #returns 10
A[c(1,3), c(2,4)]
A[1:3, 2:4] #note the same as previous
A[1:2,]
A[,1:2]
A[1,] #returns a vector
A[-c(1,3),] #"-" means "not including", in this case exclude rows 1 and 3
dim(A)

#loading data
#Auto = read.table("Auto.data", header = T, na.strings = "?")
Auto = read.csv("Auto.csv", na.strings = "?") 
#na.strings defines character/s for missing values but doesn't make any change to them
View(Auto) #view a matrix-like R object in a spreadsheet window
dim(Auto) #397 x 9
Auto[1:4,]

Auto = na.omit(Auto)
dim(Auto) #392 x 9 - omitted lines with missing values
names(Auto)

plot(Auto$cylinders, Auto$mpg)
attach(Auto)
plot(cylinders, mpg) #scatterplot

cylinders = as.factor(cylinders) #change numeric variable to categorical
plot(cylinders, mpg) #now boxplots are plotted (variable in x axis is categorical)

plot(cylinders, mpg, col = "red", varwidth = T, horizontal = T, xlab = "cylinders", 
     ylab = "MPG")

hist(mpg, col = 2, breaks = 15) #col 2 is red
#note: freq = F will output a density plot. Then, use curve() to add a density curve on top
#example: curve(dnorm(x, mean=mean(my_data), sd=sd(my_data)), add=T, col="darkblue", lwd=2)

pairs(Auto) #creates a scatterplt  matrix for all Auto variables
pairs(~mpg + displacement + horsepower + weight + acceleration, Auto) #only for a subset

plot(horsepower, mpg)
identify(horsepower, mpg, name) #returns index of the point/s clicked on plot, and prints
#variable "name" on plot for each click. Press Esc after clicking to see index value/s.

summary(mpg)

#other functions (redundant in R Studio):
#q() to quit R
#savehistory() to save workspace
#loadhistory() to load workspace



