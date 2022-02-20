data <- read.table("datas/ashina.txt", header = TRUE)
data

summary(data)
par(mfrow=c(1,2))
boxplot(data[,1],data[,2], names = c("Active","Placebo"))
plot(data[,1],data[,2]) ; abline(0,1)


# Based on the plots, we expect the active medicine to yield better pain relief.

func <- function(x,y) {mean(x-y)}
B = 1000
Tstar = numeric(B)

for (i in 1:B) {
  
  ashinastar = t(apply(cbind(data[,1],data[,2]),1,sample))
  Tstar[i] = func(ashinastar[,1],ashinastar[,2])
  
}

diff.mean = func(data[,1],data[,2])  ; diff.mean

hist(Tstar)

pl = sum(Tstar<diff.mean)/B
pr = sum(Tstar>diff.mean)/B
pl ; pr

p = 2*min(pl,pr) ; p



# Correlation tests

peruvians <- read.table("datas/peruvians.txt", header=TRUE); peruvians

attach(peruvians)
plot(systolic~weight)

cor.test(systolic, weight)

# Check the normality assumption on the two samples
par(mfrow=c(1,2))
qqnorm(weight, main="QQ Plot Weight")
qqnorm(systolic, main="QQ Plot Systolic")

# Spearman's test
cor.test(systolic, weight, method = "spearman",exact = FALSE)


# Two independent data samples
# scan -> Read data into a vector or list from the console or file.
light1 = scan("datas/light1.txt")
light2 = scan("datas/light2.txt")

par(mfrow=c(1,3))
hist(light1) ; hist(light2) ; boxplot(light1,light2)


# two samples t-test
t.test(light1, light2)
par(mfrow=c(1,2))
qqnorm(light1) ; qqnorm(light2)
# normality of the second sample is doubtful

# Mann-Whitney test
 wilcox.test(light1,light2)

# Kolmogorov-Smirnov test
hist(light1) ; hist(light2) 
