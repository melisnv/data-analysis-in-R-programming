data <- c(15.4, 17.9, 19.0, 0.5, 15.9, 2.7, 6.2, 2.5, 4.7, 6.9, 10.8, 24.3, 5.6, 23.0, 10.7)
data

#normalize.data <- function(x) {x / sqrt(sum(x^2))}
#data <- normalize.data(data) ; data

n <- length(data) ; n

# checking normality of the data

par(mfrow=c(1,3))
hist(data)
boxplot(data)
qqnorm(data)

# The sample size (15) is small and this leads us having a histogram is not very steady 
# and close to genuine density, and the QQ-plot is slightly straight in the center
# with just minor variance and space in the middle and around the corners.

mean.of.data <- mean(data) ; mean.of.data
sd.of.data <- sd(data) ; sd.of.data

# Construct a 97%-CI for mu. Evaluate the sample size needed to provide that the length 
# of the 97%-CI is at most 2. Compute a bootstrap 97%-CI for mu and compare it to the above CI. 

# p̂ = mu = 0.21 (mean of the data)
# 1-p̂ = 0.79
# Confidence interval of 2 means 2E = 0.02, then E = 0.01
# for %97 interval ; qnorm(0.97,mean=0.21,sd=0.14)

minimal.sample.size <- qnorm(0.97)^2*0.21*0.79 / (0.01)^2
minimal.sample.size # 5868.521

# The sample size needed to provide that the length of the 97%-CI is at least 5869

# Compute a bootstrap 97%-CI for mu and compare it to the above CI.
# We use simulation to find the distribution of the estimating statistic T(X). random variable Tstar(T*)
# confidence level for alpha=0.05 compute T*(1-alpha/2)= 0.975 T*(alpha/2)= 0.025 

B = 1000
Tstar = numeric(B) # create an empty numeric

for (i in 1:B) {
  Xstar = sample(data,replace = TRUE) # generating a surrogate data-set
  Tstar[i] = mean(Xstar)
}

Tstar25 = quantile(Tstar, 0.025) # computing T*(alpha/2)
Tstar975=quantile(Tstar, 0.975) # computing T*(1-alpha/2)

c(2*mean.of.data - Tstar975, 2*mean.of.data - Tstar25) # computing (2T-T*(1-alpha/2),2T-T*(alpha/2))

# Result : The Bootstrap CI is [0.14,0.28]
#     97.5%      2.5% 
#  0.1419060 0.2850332


# t-test
# The doctor claims that the mean waiting time is less than 15 minutes. 
# Under an assumption, verify this claim by a relevant t-test.

t.test(data, mu=15, alt="g") 

# One Sample t-test

# data:  data
# t = -1.968, df = 14, p-value = 0.9654
# alternative hypothesis: true mean is greater than 15
# 95 percent confidence interval:
#  7.559091      Inf
# sample estimates:
#   mean of x 
# 11.07333 
# The mean waiting time is not greater than 15.

# Propose and perform a suitable sign tests for this problem.
# Sign test	is categorical and quantitative. Can be used in place of One-sample t-test.

# Propose a way to compute the powers of the t-test and sing test from b) at mu = 14 and mu = 13, comment. 

power.signtest <- numeric(B)
power.ttest <- numeric(B)

for (i in 1:n) {
  power.ttest[i] = t.test(data, mu=14)[[3]]; # 0.1645262
  power.signtest[i] = binom.test(sum(data>15),n)[[3]];
}

# 	Exact binomial test
# data:  sum(data > 14) and B
# number of successes = 6, number of trials = 1000, p-value < 2.2e-16
# alternative hypothesis: true probability of success is not equal to 0.5
# 95 percent confidence interval:
#   0.002204982 0.013013423
# sample estimates:
#   probability of success 
# 0.006 

# One Sample t-test
# data:  data
# t = -1.4668, df = 14, p-value = 0.1645
# alternative hypothesis: true mean is not equal to 14
# 95 percent confidence interval:
#   6.793962 15.352705
# sample estimates:
#   mean of x 
# 11.07333 

sum(power.signtest<0.05)/B # 0.985
sum(power.ttest<0.05)/B # 0.985
