# ---------------------------------------------------
# Distributions and t-confidence level

library(distributions3)

x <- c(3, 7, 11, 0, 7, 0, 4, 5, 6, 2) ; x
n <- length(x) ; n

t_9 <- StudentsT(df=9) ; t_9

mean(x) + quantile(t_9, 0.12/2) * sd(x) / sqrt(n) # 2.631598
mean(x) + quantile(t_9, 1 - 0.12/2) * sd(x) / sqrt(n) # 6.368402

# Confidence interval (Cl) is (2.63, 6.37)

t.test(x, conf.level = 0.88)
#	One Sample t-test

# data:  x
# t = 4.1367, df = 9, p-value = 0.002534
# alternative hypothesis: true mean is not equal to 0
# 88 percent confidence interval:
#  2.631598 6.368402
# sample estimates:
#   mean of x 
# 4.5 
