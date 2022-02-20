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

# Construct a 97%-CI for μ Evaluate the sample size needed to provide that the length 
# of the 97%-CI is at most 2. Compute a bootstrap 97%-CI for μ and compare it to the above CI. 

# p̂ = mu = 0.21 (mean of the data)
# 1-p̂ = 0.79
# Confidence interval of 2 means 2E = 2, then E = 1
# for %97 interval ; qnorm(0.97,mean=0.21,sd=0.14)

minimal.sample.size <- qnorm(0.97)^2*(0.21)^2*0.79 / (1)^2
minimal.sample.size # 0.5868521

# The sample size needed to provide that the length of the 97%-CI is at least 0.587

# --------------------------------------------------------------------------------------------
# a
# Compute a bootstrap 97%-CI for μ and compare it to the above CI.
# We use simulation to find the distribution of the estimating statistic T(X). random variable Tstar(T*)
# confidence level for alpha=0.05 compute T*(1-alpha/2)= 0.975 T*(alpha/2)= 0.025 

B = 1000
Tstar = numeric(B) # create an empty numeric

for (i in 1:B) {
  Xstar = sample(data,replace = TRUE) # generating a surrogate data-set
  Tstar[i] = mean(Xstar)
}

Tstar25 = quantile(Tstar, 0.015) # computing T*(alpha/2)
Tstar975=quantile(Tstar, 0.985) # computing T*(1-alpha/2)

c(2*mean.of.data - Tstar975, 2*mean.of.data - Tstar25) # computing (2T-T*(1-alpha/2),2T-T*(alpha/2))

# Result : The Bootstrap CI is [0.14,0.28]
#     97.5%      2.5% 
#  0.1419060 0.2850332

# --------------------------------------------------------------------------------------------
# b
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

# --------------------------------------------------------------------------------------------
# c
# Propose and perform a suitable sign tests for this problem.
# Sign test	is categorical and quantitative. Can be used in place of One-sample t-test.

# Propose a way to compute the powers of the t-test and sing test from b) at μ = 14 and μ = 13, comment. 

stest.power <- binom.test(sum(data<14),n,p=0.5)[[3]] ; stest.power # 0.607
ttest.power <- t.test(data, mu=14,alt="l")[[3]] ; ttest.power # 0.082

# The power in mu = 14 for the t-test (0.165) is lower than the sign test (0.607).

stest.power_2 <- binom.test(sum(data>13),n,p=0.5)[[3]] ; stest.power_2 # 0.607
ttest.power_2 <- t.test(data, mu=13,alt="g")[[3]] ; ttest.power_2 # 0.824

# The power in mu = 13 for the t-test (0.824) is higher than the sign test (0.607).

# --------------------------------------------------------------------------------------------
# d
# Let p be the probability that a patient has to wait longer than 15.5 minutes.
# The (1-α)-confidence interval for p is p̂±zα/2*sqrt(p̂(1-p̂)/n)

# --------------------------------------------------------------------------------------------
# e

waiting.time <- data ; waiting.time
sum(waiting.time > 15.5)

more.than.fifteen <- data[data>15.5] ; more.than.fifteen

binom.test(sum(data>15.5),n,p=0.5) # people who waits more than 15.5

# 	Exact binomial test

# data:  sum(data > 15.5) and n
# number of successes = 5, number of trials = 15, p-value = 0.3018
# alternative hypothesis: true probability of success is not equal to 0.5
# 95 percent confidence interval:
#   0.1182411 0.6161963
# sample estimates:
#  probability of success 
#    0.3333333 

binom.test(3,n,p=0.5) # 3 men who waits more than 15.5
# 	Exact binomial test

# data:  3 and n
# number of successes = 3, number of trials = 15, p-value = 0.03516
# alternative hypothesis: true probability of success is not equal to 0.5
# 95 percent confidence interval:
#  0.04331201 0.48089113
# sample estimates:
#   probability of success 
# 0.2 

binom.test(sum(data<=15.5),n,p=0.5) # people who waits less than 15.5
# 	Exact binomial test

# data:  sum(data <= 15.5) and n
# number of successes = 10, number of trials = 15, p-value = 0.3018
# alternative hypothesis: true probability of success is not equal to 0.5
# 95 percent confidence interval:
#  0.3838037 0.8817589
# sample estimates:
#  probability of success 
# 0.6666667 

binom.test(6,n,p=0.5) # 6 women who waits less than 15.5
# Exact binomial test

# data:  6 and n
# number of successes = 6, number of trials = 15, p-value = 0.6072
# alternative hypothesis: true probability of success is not equal to 0.5
# 95 percent confidence interval:
#   0.1633643 0.6771302
# sample estimates:
#   probability of success 
# 0.4 