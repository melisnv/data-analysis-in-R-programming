# To improve rain fall in dry areas, an experiment was carried out with 52 clouds. Scientists investigated whether the addition of silver nitrate leads to more rainfall. 
# They chose 26 out of a sample of 52 clouds and seeded it with silver nitrate. 

data <- read.table("datas/clouds.txt", header = TRUE)
head(data) ; length.c1 <-length(c1) ;

c1 = data[,1] # seeded
c2 = data[,2] # unseeded

par(mfrow=c(1,2)) ; hist(c1,main = "Seeded Clouds") ; hist(c2,main = "Unseeded Clouds")
boxplot(c1,main="Seeded Clouds") ; boxplot(c2,main = "Unseeded Clouds")

# the two samples t-test
t.test(c1,c2)

# Welch Two Sample t-test

# data:  c1 and c2
# t = 1.9984, df = 33.856, p-value = 0.05375
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -4.740491 559.585876
# sample estimates:
#   mean of x mean of y 
# 441.9846  164.5619 

# the Mann-Whitney test
wilcox.test(c1,c2)

# 	Wilcoxon rank sum test with continuity correction

# data:  c1 and c2
# W = 473, p-value = 0.01383
# alternative hypothesis: true location shift is not equal to 0

#  Kolmogorov-Smirnov test
par(mfrow = c(1,2)) ; hist(c1,main = "Seeded") ; hist(c2,main = "Unseeded")
ks.test(c1,c2)

# 	Two-sample Kolmogorov-Smirnov test

# data:  c1 and c2
# D = 0.42308, p-value = 0.01905
# alternative hypothesis: two-sided

# --------------------------------------------------------------------------------------------
# b
sq.data <- sqrt(data) ; sq.data 
sq.of.sq.data <- sqrt(sq.data) ; sq.of.sq.data

seeded1 = sq.data[,1] ; seeded2 = sq.of.sq.data[,1] ; seeded1 ; seeded2
unseeded1 = sq.data[,2] ; unseeded2 = sq.of.sq.data[,2] ; unseeded1 ; unseeded2

t.test(seeded1, unseeded1)
t.test(seeded2, unseeded2)

t.test(seeded1, unseeded1, paired = TRUE) # it's essential to add paired = TRUE
t.test(seeded2, unseeded2, paired = TRUE)

wilcox.test(seeded1,unseeded1)
wilcox.test(seeded2,unseeded2)

ks.test(seeded1,unseeded1)
ks.test(seeded2,unseeded2)

# --------------------------------------------------------------------------------------------
# c
# As test statistic the maximum of the sample data will be used

mean.c1 <- mean(c1) ; mean.c1

par(mfrow=c(1,2))
hist(c1, prob = T,main = "Seeded Clouds") ; hist(c2, prob = T,main = "Unseeded Clouds") 

M = 1000
Tstar = numeric(M)

for (i in 1:M) {
  Xstar = sample(c1,replace = TRUE) ; # surrogate vector
  Tstar [i] = mean(Xstar)
}

Tstar.25 <- quantile(Tstar,0.025)
Tstar.975 <- quantile(Tstar,0.975)

sum(Tstar<Tstar.25)
c(2*mean.c1-Tstar.975,2*mean.c1-Tstar.25)
#   97.5%     2.5% 
# 179.1093 650.9029 
# The 95% CI for the seeded clouds is [179,651], it's mean is 442

# By using a bootstrap test with the test statistic