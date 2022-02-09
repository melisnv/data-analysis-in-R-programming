x = 3
x

x == 3 ; x == 4

# a vector called y is consists a concatenation function c  which concatanes 
# objects x and 10
y = c(x,10)
y

1:10
seq(4,30,2)
rep(2,5)
rep(1:3,5) # repeats numbers between 1 to 3 5 times

m = c(7.3,9.2,3.4,5.2,6.4,8.1)
m
length(m) # 6

# Referencing
m[3] # 3.4
m[-1]
# To find the locations within the vector y that satisfy some condition,
# such as those elements which are greater than 2, we would use the command;
which(m > 6)


# The matrix function can be used to create a matrix in R. We need to provide a vector containing the
# elements of the matrix, and specify either the number of rows or the number of columns of the matrix. This
# number should divide evenly into the length of the vector, or we will get a warning.

matrix(1:6,nrow = 2)
matrix(1:6,nrow = 3)


# If we prefer to fill in the matrix row-by-row, activate the byrow
M = matrix(1:10, ncol = 5, byrow = TRUE)
M

# Creating a matrix by binding vectors or matrices together either 
# row-wise or column-wise using the rbind or the cbind functions
dim(M) # returns the dimension
nrow(M)
ncol(M)

# to refer to the element M(2,3)
M[2,3]
# to refer to the element of the 4th column
M[,4]
# to refer the first row and first two columns
M[1,1:2]

# To obtain the transpose of a matrix, we use the transpose function
t(M)

N = matrix(10:19,nrow = 5)
N

dim(M) ; dim(N)

# To multiply two matrices with compatible dimensions use %*%
multp = M%*%N
multp # 2x2


# To find the determinant of a square matrix M, use the determinant function
det(multp) # input must be a square matrix

# To obtain the inverse of an invertible square matrix M
solve(multp)

# ---------------------------------
# Plotting
y
boxplot(y)
z = c(2.7,4.3,9.5,1.4,5.5,7.2)
boxplot(y,z)

x = seq(0,10,length=30) ; x

y = sin(x); y

plot(x,sin(x))
plot(x,sin(x), type = 'l', xlab = "x",ylab = "sin(x)")

x = seq(0,10,length=300) ; x
y = seq(10,20,length=300) ; y

# The setting lty=2 changes it from a solid line to a dashed line,
# and the setting lwd=2 doubles the line width.
plot(x,y, main = "Plot",xlab="x",ylab="y", pch = 25,cex = 1.5,col="blue")
abline(v=1, lty=2, lwd =2, col='pink')
abline(h=16, lty=1, lwd= 3, col='orange')


# To add a plot of a function to the existing plot over a certain range,
# first create a vector consisting of many points in this range and
# compute the vector of their function values:

z <- seq(from=0, to =1.2, by=0.01) ; z
f = seq(from=2, to =3.2, by=0.01) ; f

lines(z,f,col='green',lwd = 1.5)


# Arithmetic and build-in R functions
2*3-7 ; 2^3
y^3

mean(y)
# var, cov and cor compute the variance of x and 
# the covariance or correlation of x and yif these are vectors. 
# If x and y are matrices then the covariances(or correlations) 
# between the columns of x and the columns of y are computed.
var(y)

sum((y-mean(y)^2)/(length(y)-1))
# Sorting a vector or factor into ascending or descending order.
sort(y)
# Takes a sample of the specified size from the elements of x using either with or without replacement.
sample(y,3)

# ---------------------------------
# Simulating data
# A random number generator simulates samples from standard distributions (normal, uniform, chisquare,binomial)
x = rnorm(100) ; x
m = dnorm(20) ; m

hist(x, prob= TRUE)

s = rbinom(1,30,0.5); s
?rbinom()

# --------------------------------------------
# Loops
# rexp(m, r)—Returns a vector of m random numbers having the exponential distribution
t = numeric(500); t
for (i in 1:500) {
  t[i] = mean(rexp(25))
}
t

hist(t)
hist(t, prob=TRUE)
# rescale the vertical axis of the histogram so that the area is 1,
# and add a best fitting normal density curve
u = seq(min(t),max(t),length=100)
lines(u,dnorm(u,mean(t),sqrt(var(t))))

hist(rexp(500))

# --------------------------------------------
# Reading data from files

data = read.table(file = "./datas/mortality.txt", header=TRUE)
head(data)
tail(data)
str(data)
summary(data)

dim(data) # the numbers of rows and columns in your data

data$teen

data$teen[4]
data[,'teen'] # same with "data$teen"

# filtering the data
is.data.frame(data)

data[data$teen >10,]
data[1:5,3]

# -----------------------------------------
# Data types :
# Basic data types in R include numeric for numbers, character for letters,
# and factor for categories of nominal variables.

labels = 1:10 ; labels
sum(labels)
typeof(labels) # integer


# The vector 1:10 consists of the numbers 1 to 10 and labels is therefore of type numeric.
# In order to use them as labels of a categorical variable, coerce it to type factor.
# The function factor is used to encode a vector as a factor
labels = as.factor(labels)
sum(labels)
# Error in Summary.factor(1:10, na.rm = FALSE) : 
# ‘sum’ not meaningful for factors
is.factor(labels) # TRUE

# -----------------------------------------
# Histograms and QQ-plots

x = rnorm(30) ; x
# Multiple graphs can be putted in a single plot by setting
# some graphical parameters with the help of par() function.
# The mfrow() parameter allows to split the screen in several panels. 
par(mfrow=c(1,2))
hist(x)
qqnorm(x)


hist(10*x+3)
qqnorm(10*x+3)

x = rnorm(10) ; x
hist(x)
qqnorm(x)

x = runif(100) ; x
hist(x)
qqnorm(x)

# The function rchisq samples from yet another population distribution, 
# the chisquare distribution, in this case with 5 degrees of freedom.
x = rchisq(30,5) ; x
hist(x)
qqnorm(x)


# -----------------------------------------
# Two-sample t-test:
# Simulating possible outcomes of the two-sample
# t-test for comparing, say, heights of men and women.

n=30; m=30; mu=180; nu=175; sd=10
x = rnorm(n,mu,sd) ; y = rnorm(m,nu,sd)
t.test(x,y,var.equal = TRUE)
t.test(x,y,var.equal = TRUE)[[3]]

# The t.test command produces a small report on the test. 
# This includes the p-value for testing the null hypothesis 
# that the population means of the two populations are equal.
# It also gives a confidence interval for the difference of the population means.

n=m=30; mu=180; nu=175; sd=10; B=1000; p=numeric(B)
for (b in 1:B) {x=rnorm(n,mu,sd); y=rnorm(m,nu,sd)
p[b]=t.test(x,y,var.equal=TRUE)[[3]]}
power=mean(p<0.05)
