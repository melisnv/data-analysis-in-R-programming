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


# -------------------------------------------------------------
# creating artificial data

mu = 0.2 ; x = rnorm(50,mu,1)
t.test(x, mu=0)

# 	One Sample t-test

# data:  x
# t = 2.1408, df = 49, p-value = 0.0373
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#  0.018992 0.600900
# sample estimates:
#   mean of x 
# 0.309946 

# H0 is rejected because |t|=2.14 > qt(0.975,49) and p-value (0.0373) < 0.05 
quantile <- qt(0.975,49) ; quantile # 2.009575

# -----------------------------------------------------------
# Binomial Test

# In a sample of 100 trains arriving at Amsterdam Central station, a sample proportion 
# p = 0.89(89/100) observed to arriving in time. We want to test this is significantly 
# lower than the reported 95% for the Netherlands.
# Hence, Ho : p > 0.95 versus H1 : p < 0.95

binom.test(89,100,0.95, alternative = "less")
#    Exact binomial test

# data:  89 and 100
# number of successes = 89, number of trials = 100, p-value = 0.01147
# alternative hypothesis: true probability of success is less than 0.95
# 95 percent confidence interval:
#   0.0000000 0.9370754
# sample estimates:
#   probability of success : 0.89 

prop.test(89,100,0.95, alternative = "less")
#   1-sample proportions test with continuity correction

# data:  89 out of 100, null probability 0.95
# X-squared = 6.3684, df = 1, p-value = 0.005808
# alternative hypothesis: true p is less than 0.95
# 95 percent confidence interval:
#   0.000000 0.935426
# sample estimates:
#  p : 0.89 


prop.test(89,100,0.95, alternative = "two.sided")
binom.test(89,100,0.95, alternative = "two.sided")


# We want to test about the difference in population success proportion p1 and p2.
# sample proportion of A of size 1000
p.a <- 20/1000 ; p.a

# sample proportion of B of size 1500
p.b <- 19/1500 ; p.b

prop.test(c(20,19),c(1000,1500))

# 	2-sample test for equality of proportions with continuity correction

# data:  c(20, 19) out of c(1000, 1500)
# X-squared = 1.6508, df = 1, p-value = 0.1989
# alternative hypothesis: two.sided
# 95 percent confidence interval:
#  -0.00385957  0.01852624
# sample estimates:
#  prop 1     prop 2 
# 0.02000000 0.01266667 

# Do not reject the hypothesis p.a = p.b

mu = nu = 0 ;  t=numeric(100000)
for (i in 1:100000) {
  x = rnorm(50,mu,1);
  y = rnorm(50,nu,1);
  t[i] = t.test(x,y)[[1]]
}

sum(abs(t) >= abs(-2.4339))/length(t)

# t-test vs sign test

B = 1000 ; n=50
psign = numeric(B) # will contain p-values of sign test
pttest = numeric(B) # will contain p-values of t-test

for (i in 1:B) {
  x = rnorm(n,mean=0.5,sd=1) 
  pttest [i] = t.test(x)[[3]]
  psign [i] = binom.test(sum(x>0), n,p=0.5)[[3]]
}


sum(psign<0.05)/B   # 0.751
sum(pttest<0.05)/B  # 0.923
