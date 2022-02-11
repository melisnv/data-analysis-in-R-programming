---
  title: "Assignment 0"
author: "Melis Nur Verir, Vrije Universiteit Amsterdam"
date: "11 February 2022"
output: pdf_document
highlight: tango
---
  
# Generate two samples of sizes 100 and 100000 from a standard normal distribution. 
# Make histograms and QQ-plots, compute the means and standard deviations of the both samples.

a = rnorm(100) ; a
b = rnorm(100000) ; b

# means
mean(a)
mean(b)

# standard deviations
sd(a)
sd(b)

par(mfrow=c(1,2))
hist(a)
qqnorm(a)
hist(b)
qqnorm(b)

# For a standard normal distribution, compute the following 3 probabilities: 
# that an arbitrary outcome is smaller than 2, that it is bigger then -0.5 and 
# that it is between -1 and 2.

n = 100
options(digits=3)
amount1 = sum(a<2) ; amount1
probability1 = amount1/n ; probability1

amount2 = sum(a >(-0.5)) ; amount2
probability2 = amount2/n ; probability2

amount3 = sum(a>-1 & a<2) ; amount3
probability3 = amount3/n ; probability3

# Repeat a) and b) for a normal distribution with mean=3 and sd=2. 
# Find also the value such that 95% of the outcomes is smaller than that value.

c = rnorm(100,3,2) ; c
d = rnorm(100000, 3,2) ; d

mean(a) ; mean(c)
mean(b); mean(d)

sd(a) ; sd(c)
sd(b) ; sd(d)

par(mfrow=c(1,2)) ; hist(c) ; hist(d)
qqnorm(e) ; qqnorm(d)


# Generate in this way a sample of size 1000 from a normal distribution with mean=-10
# and sd= 5. Verify that the sample mean and standard deviation are close to the true values.

e = rnorm(1000,-10,5) ; e

mean(e) # -9.883679 is close to -10
sd(e) # 5.073855 is close to 5 but sligthly bigger than


