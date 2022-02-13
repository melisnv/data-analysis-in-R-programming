data = read.table("datas/expensescrime(1).txt", header = TRUE)
head(data)

summary(data)
str(data)

# ------------------------------------------
# t-test

x <- data$crime ; x
n <- length(x) ; n

t.test(x,mu=4500,alternative = "g")
#  One Sample t-test

# data:  x
# t = 1.5583, df = 50, p-value = 0.06273
# alternative hypothesis: true mean is greater than 4500
# 95 percent confidence interval:
#   4477.224      Inf
# sample estimates:
#   mean of x 
# 4801.843 

# Why it's INF ?
