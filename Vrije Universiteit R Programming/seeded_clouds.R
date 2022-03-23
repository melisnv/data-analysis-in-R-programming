# To improve rain fall in dry areas, an experiment was carried out with 52 clouds. Scientists investigated whether the addition of silver nitrate leads to more rainfall. They chose 26 out of a sample of 52 clouds and seeded it with silver nitrate. 
#Test whether silver nitrate has an effect by performing three tests: the two samples t-test (argue whether the data are paired or not), the Mann-Whitney test and the Kolmogorov-Smirnov test. Indicate whether these tests are actually applicable for our research question. Comment on your findings. 

data <- read.table("datas/clouds.txt", header = TRUE)
head(data) ;

c1 = data[,1] # seeded 
c2 = data[,2] # unseeded
length.c1 <-length(c1) ;

par(mfrow=c(1,2)) ; hist(c1,main = "Seeded Clouds") ; hist(c2,main = "Unseeded Clouds")
boxplot(c1,main="Seeded Clouds") ; boxplot(c2,main = "Unseeded Clouds")

shapiro.test(c1) ; shapiro.test(c2)

# the two samples t-test assumes that both samples come from a normal population.
t.test(c1,c2)

qqnorm(c1) ; qqnorm(c2) # Normality of both samples is doubtful


# the Mann-Whitney test is based on ranks but not assume normality
wilcox.test(c1,c2)
# p-value of 0.01 < 0.05, hence Ho is rejected.

#  Kolmogorov-Smirnov test assume normality
ks.test(c1,c2)
# p-value of 0.02 < 0.05, hence Ho is rejected.


# Repeat the procedures from a) first on the square root of the values in clouds.txt, then on the square root of the square root of the values in clouds.txt. Comment on your findings.
sq.data <- sqrt(data) ; sq.data 
sq.of.sq.data <- sqrt(sq.data) ; sq.of.sq.data

seeded1 = sq.data[,1] ; seeded2 = sq.of.sq.data[,1] ; seeded1 ; seeded2
unseeded1 = sq.data[,2] ; unseeded2 = sq.of.sq.data[,2] ; unseeded1 ; unseeded2

par(mfrow=c(1,2)) ; hist(seeded1,main = "Seeded Sqrt Clouds") ; hist(unseeded1,main = "Unseeded Sqrt Clouds")
boxplot(seeded1,main="Seeded Clouds") ; boxplot(unseeded1,main = "Unseeded Clouds")
shapiro.test(c1) ; shapiro.test(c2)

par(mfrow=c(1,2)) ; hist(seeded1,main = "Seeded DoubSqrt") ; hist(unseeded1,main = "Unseeded DoubSqrt")
boxplot(seeded2,main="Seeded Clouds") ; boxplot(unseeded2,main = "Unseeded Clouds")
shapiro.test(seeded2) ; shapiro.test(unseeded2)


t.test(seeded1, unseeded1)
t.test(seeded2, unseeded2)

t.test(seeded1, unseeded1, paired = TRUE) # it's essential to add paired = TRUE
t.test(seeded2, unseeded2, paired = TRUE)

wilcox.test(seeded1,unseeded1)
wilcox.test(seeded2,unseeded2)

ks.test(seeded1,unseeded1)
ks.test(seeded2,unseeded2)


#  Assuming  X1...X26 ~ Exp(λ) and using the central limit theorem, find an estimate λ^ of λ and construct a 95%-CI for λ.

mean.c1 <- mean(c1) ; mean.c1

par(mfrow=c(1,2))
hist(c1, prob = T,main = "Seeded Clouds") ; hist(c2, prob = T,main = "Unseeded Clouds") 

M = 1000
Tstar = numeric(M)

for (i in 1:M) {
  Xstar = rexp(length.c1,1) ; # surrogate vector of Exp(λ)
  Tstar[i] = mean(Xstar)
}

Tstar.25 <- quantile(Tstar,0.025)
Tstar.975 <- quantile(Tstar,0.975)

sum(Tstar<Tstar.25)
c(2*mean.c1-Tstar.975,2*mean.c1-Tstar.25)
#   97.5%     2.5% 
#  882.5396 883.3353
# The 95% CI for the seeded clouds is [882,883], it's mean is 441

# By using a bootstrap test with the test statistic T =median(X1,...X26) test the hypothesis Ho : X1...X26 ~ Exp(λo) with the parameter λo=λ^

T.star <- numeric(M)
median.data <- median(c1) ; median.data
for (i in 1:M) {
  X.star <- rexp(length.c1,1)
  T.star[i] <- median(X.star)
}

hist(Tstar,prob=T)
pl <- sum(T.star<median.data)/M ; pr <- sum(T.star> median.data)/M
p <- 2*min(pl,pr)
pl ; pr ; p

# Test this also by the Kolmogorov-Smirnov test
ks.test(c1, "pnorm")
# Since the p-value(2.2e-16) is less than .05, we reject the null hypothesis. We have sufficient evidence to say that the sample data does not come from a normal distribution.


# Using an appropriate test, verify whether the median precipitation for seeded clouds is less than 300. Next, design and perform a test to check whether the fraction of the seeded clouds with the precipitation less than 30 is at most 25%.

meadian.data <- median(data$seeded) ; median.data

binom.test(sum(data$seeded < 300), 26, p=0.5, alt='l') # p-value = 0.9622

binom.test(sum(c1<30), 26, p=0.25, alt='l') # p-value = 0.08019
