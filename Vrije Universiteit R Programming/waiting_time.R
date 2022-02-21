data <- c(15.4, 17.9, 19.0, 0.5, 15.9, 2.7, 6.2, 2.5, 4.7, 6.9, 10.8, 24.3, 5.6, 23.0, 10.7)
data

n <- length(data) ; n

# checking normality of the data
shapiro.test(data)
# we can assume normality since the p-value (0.32) > 0.05 implies that the distribution of the data are not significantly different from normal distribution.
par(mfrow=c(1,3))
hist(data)
boxplot(data)
qqnorm(data)

# The sample size (15) is small and this leads us having a histogram is not very steady 
# and close to genuine density, and the QQ-plot is slightly straight in the center
# with just minor variance and space in the middle and around the corners.

mean.waiting.time <- mean(data) ; mean.waiting.time
sd.of.data <- sd(data) ; sd.of.data

# Construct a 97%-CI for μ Evaluate the sample size needed to provide that the length 
# of the 97%-CI is at most 2. Compute a bootstrap 97%-CI for μ and compare it to the above CI. 

tscore <- (qt(0.97,df=n-1)*(sd.of.data))/sqrt(n) ; tscore

margin <- 1
z.score <- qnorm(0.97)
minimal.sample.size <- ((z.score^2)*(sd.of.data^2))/(margin^2)
minimal.sample.size

# The sample size needed to provide that the length of the 97%-CI is at least 211.2

# Compute a bootstrap 97%-CI for μ and compare it to the above CI.

B = 1000
Tstar = numeric(B) # create an empty numeric

for (i in 1:B) {
  Xstar = sample(data,replace = TRUE) # generating a surrogate data-set
  Tstar[i] = mean(Xstar)
}

Tstar15 = quantile(Tstar, 0.015) # computing T*(alpha/2)
Tstar985=quantile(Tstar, 0.985) # computing T*(1-alpha/2)

c(2*mean.waiting.time - Tstar985, 2*mean.waiting.time - Tstar15) # computing (2T-T*(1-alpha/2),2T-T*(alpha/2))


# The doctor claims that the mean waiting time is less than 15 minutes. Under an assumption, verify this claim by a relevant t-test.

t.test(data, mu=15, alt="g")
qt(0.975,14)
# For this data, R-output estimates X ≡ 11, the 95% CI [7.559091,Inf], p-value ≡ 0.965. Ho : μ < 15 not rejected because :
# 1) |t| = 1.97 < 2.14 [qt(0.975, df=14)]
# 2) p-value = 0.965 > 0.05


# Propose and perform a suitable sign tests for this problem.
# Sign test	is categorical and quantitative. Can be used in place of One-sample t-test.
binom.test(sum(data<15),14,p=0.5, alt="g")

# Test based on rank
# Wilcoxon rank-sum test (for that population cannot be assumed to be normally distributed)
# Mann-Whitney U-test (greater efficiency on non-normal distributions, and it is nearly efficient on normal distributions)

# Propose a way to compute the powers of the t-test and sing test from b) at μ = 14 and μ = 13, comment. 

compute.powers <- function(B,n,mu,sd,data) {
  power.sign <- numeric(B)
  power.ttest <- numeric(B)
  
  for (i in 1:B) {
    
    x = rnorm(n,mu,sd)  # generate data under H1 with requested mean
    power.ttest[i] <- t.test(x)[[3]] # extracts p-value
    power.sign[i] <- binom.test(sum(data<mu),n,p=0.5)[[3]] # extracts p-value
    }
  
  power.in.sign.test <- sum(power.sign < 0.05)/B
  power.in.t.test <- sum(power.ttest < 0.05)/B
  
  print(power.in.sign.test)
  print(power.in.t.test)
}

compute.powers(1000,50,14,1,data)
compute.powers(1000,50,13,1,data)


# Let p be the probability that a patient has to wait longer than 15.5 minutes.
# The (1-α)-confidence interval for p is p̂±zα/2*sqrt(p̂(1-p̂)/n)
# TA : Calculate the margin with error with alpha and in order to calculate it exploit the pr= 0.53

lower.bound <- 0.53 # pr
margin.error <- mean.waiting.time - lower.bound ; margin.error

length.of.data <- length(data)
sd.error <- sd.of.data / sqrt(length.of.data) ; sd.error

t.score <- margin.error / sd.error ; t.score # 5.284

# The researcher also reported that there were 3 men and 2 women among 5 patients who had to wait more than 15.5 minutes, 4 men and 6 women among the remaining 10 patients.
# TA : creating a short matrix

new.data <- data.frame(waiting.time = as.vector(as.matrix(data)), gender = factor(sample(rep(1:2, c(7,8)))))
new.data

# check for appopriate test