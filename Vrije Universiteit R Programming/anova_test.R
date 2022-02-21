# randomization in R

I = 4 ; N=5
rep(1:I,N)
sample(rep(1:I,N))
# 4 1 2 2 3 3 2 4 2 4 3 1 3 1 3 4 1 2 4 1

data <- read.table("datas/melon.txt",header = TRUE)
data

par(mfrow=c(1,2)) ; boxplot(data,main="Melon Data") ; stripchart(data,vertical = TRUE)

# create a data frame with a numeric column of responses and second factor column of the corresponding factor levels
# make sure that 2nd column is a factor
dataframe.melon <- data.frame(yields=as.vector(as.matrix(data)), variety=factor(rep(1:4,each=6)))
dataframe.melon[1:5,]
dataframe.melon

is.factor(dataframe.melon$variety) ; is.numeric(dataframe.melon$variety)

# lm is used to fit linear models.
data.anova <- lm(yields~variety,data = dataframe.melon) ; data.anova
# Call:
# lm(formula = yields ~ variety, data = dataframe.melon)
anova(data.anova)

# Analysis of Variance Table

# Response: yields
#           Df Sum Sq   Mean Sq   F value   Pr(>F)
# variety    3  43.55   14.516   0.5543   0.6512
# Residuals 20 523.73   26.186

summary(data.anova)
fitted(data.anova)
confint(data.anova)
#                 2.5 %    97.5 %
# (Intercept) 17.755509 26.471158
# variety2    -5.191228  7.134561
# variety3    -2.639561  9.686228
# variety4    -3.744561  8.581228

# sum parametrization
contrasts(dataframe.melon$variety) = contr.sum
data.anova = lm(yields~variety, data = dataframe.melon) ; summary(data.anova)


par(mfrow=c(1,2))
qqnorm(residuals(data.anova)) ; plot(fitted(data.anova),residuals(data.anova))



# Kruskal-Wallis test ( non-parametric alternative to ANOVA)
# does not rely on normality, it is based on ranks

ratdata <- read.table("datas/ratdata.txt",header = TRUE)
ratdata # this data contains the number of worms in rats in 4 different treatment groups

par(mfrow=c(1,2)) ;  boxplot(ratdata) ; stripchart(ratdata,vertical = TRUE)

# create a data frame with the first columns containing all the outcomes and the second column that indicates the levels of factor

ratframe <- data.frame(worms=as.vector(as.matrix(ratdata)),group=as.factor(rep(1:4,each=5)))
ratframe

is.factor(ratframe$group) ; is.numeric(ratframe$group)

attach(ratframe)
kruskal.test(worms,group)

# Compare the result of a Kruskal test with an ANOVA test

rataov = lm(worms~group) ; anova(rataov) # The one way ANOVA does not yield a significant difference.
summary(rataov)

qqnorm(rataov$residuals)

# Permutation test

dogs <- read.table("datas/dogs.txt", header = TRUE) ; dogs
# the data concerns measures of plasma epinephrine in dogs for three different anesthesia drugs
treat <- factor(rep(1:3,c(10,10,10)),labels = c("iso","halo","cyclo")) # group as a factor
dogsdata <- data.frame(plasma = as.vector(as.matrix(dogs)),treat)
dogsdata

boxplot(plasma~treat, data = dogsdata) ; stripchart(dogsdata,vertical = TRUE)

attach(dogsdata)
func = function(x) sum (residuals(x)^2)
M = 1000
Tstar = numeric(M)

for (i in 1:M) {
  Treatstar = sample(treat) # permuting the labels
  Tstar[i] = func(lm(plasma~Treatstar))
  
}

res = func(lm(plasma~treat))
res # 2.72474
hist(Tstar)


pl = sum(Tstar<res)/M
pr = sum(Tstar>res)/M
pl ; pr
min(pl,pr)
