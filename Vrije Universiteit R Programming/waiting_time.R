data <- c(15.4, 17.9, 19.0, 0.5, 15.9, 2.7, 6.2, 2.5, 4.7, 6.9, 10.8, 24.3, 5.6, 23.0, 10.7)
data

normalize.data <- function(x) {x / sqrt(sum(x^2))}
data <- normalize.data(data) ; data

n <- length(data) ; n

# checking normality of the data

par(mfrow=c(1,3))
hist(data)
boxplot(data)
qqnorm(data)

# The sample size (15) is small and this leads us having a histogram is not very steady 
# and close to genuine density, and the QQ-plot is slightly straight in the centre
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
minimal.sample.size

# The sample size needed to provide that the length of the 97%-CI is at least 5869

t.test(data, conf.level = 0.97) 

# 	One Sample t-test

# data:  data
# t = 5.5499, df = 14, p-value = 7.157e-05
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   0.1313523 0.2968243
# sample estimates:
#   mean of x 
# 0.2140883 
