# Generate the following samples, and plot for each of them the histogram,boxplot and QQ-plot
# •sample of size 10000 from the lognormal distribution with mean = sd = 2,
# •sample of size 40 from the binomial distribution with n = 50 and p = 0:25,
# •sample of size 60 from the uniform distribution on the interval [-2,3],
# •sample of size 200 from the Poisson distribution with lambda= 9.


# sample 1
sample1 = rlnorm(10000,2,2) ; sample1
par(mfrow=c(1,3)) ; hist(sample1,xlim=c(0,100),breaks=1000) ; qqnorm(sample1) ; boxplot(sample1)

# sample 2
sample2 = rbinom(40,50,0.25) ; sample2 # from the binomial distribution with n=50 and p=0.25
par(mfrow=c(1,3)) ; hist(sample2) ; qqnorm(sample2) ; boxplot(sample2)

# sample 3
sample3 = runif(60,-2,3) ; sample3
par(mfrow=c(1,3)) ; hist(sample3) ; qqnorm(sample3) ; boxplot(sample3)

# sample 4 
sample4 = rpois(200,9) ; sample4 #from the Poisson distribution with lambda = 350
par(mfrow=c(1,3)) ; hist(sample4) ; qqnorm(sample4) ; boxplot(sample4)

# Except for lognormal, all are symmetric, while binomial  and Poisson appear to be normal.
# Noise is visible in small sample sizes (10,40,60). For sufficiently high sample sizes,
# histograms are more stable and provide a better representation of the underlying density.