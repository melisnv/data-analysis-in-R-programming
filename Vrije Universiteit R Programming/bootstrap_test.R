
N = 1000 # sample size
data <- numeric(N)

for (i in 1:N) {
  data[i] = rexp(1); # standard exponential distribution
}

data # the data are random sample from sed

par(mfrow=c(1,2))
hist(data, prob = T)
hist(data, prob = T, ylim = c(0,0.7))

x = seq(0,max(data),length=1000) ; x 
lines(x,dexp(x), type="l",col="red",lwd=2)

# as test statistics the maximum of the sample is used.
t = max(data) ; t # 7.323548

B = 1000
Tstar = numeric(B) ; Tstar
n = length(data)

for (i in 1:B) {
  Xstar = rexp(n,1) ; Tstar[i] = max(Xstar)
}

hist(Tstar,prob=T, ylim = c(0,0.4), main = "Histogram of T* and True Density Curve of T")
lines(density(Tstar),type = "l",col="darkgreen",lwd=2)

# p-value in Bootstrap
pL <- sum(Tstar < t)/ B ;
pR <- sum(Tstar > t)/B ;
p <- 2*min(pL,pR);
pL ; pR ; p 

# The p-value is 0.99. It is recommended to repeat a bootstrap test a few times
# to see if p-value is stable.