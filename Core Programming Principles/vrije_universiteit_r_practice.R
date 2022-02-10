x= 1:20 ; x
m = matrix(x,4,5,byrow = TRUE) ; m

m[2,3]
m[2,]
m[,3]

y = sample(1:100,20) ; y

z = x+y ; z
y = x +2*y ; y

cbind(x,y)

par("mar")
par(mar=c(1,1,1,1))
plot(x,y)
abline(100,2)

# generate a sample of size 50 from normal distribution with mean=0, std=sqrt(2)
x = rnorm(50,0,sqrt(2)) ; x
y = rnorm(50) ; y # mean = 0, std = 1

mean(x) # sample mean of the values in x
sd(x) # sample standard deviation of values in x
var(x) # sample variance of the values in x

cor(x,y) # sample correlation between x and y

# select negative elements in x
x[x < 0]
sum(x<0) # count negative numbers in x

u= seq(-5,5,0.1) ; u #sequence from -5 to 5 with 0.1 step size

v = dnorm(u,0,sqrt(2)) ; v #normal density of u with mean=0,std=sqrt(2)

lines(u,v)

{hist(x,prob=T)
+ lines(u,v)}

plot(ecdf(x)) # empirical distribut. function of x
lines(u, pnorm(u,0,sqrt(2)))


qqnorm(x)

# generate random sample of size 25 from exponential distribution with 0:25
data = rexp(25,rate=0.25) ; data

