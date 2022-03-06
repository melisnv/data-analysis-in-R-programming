# First create a table of the counts in matrix form.

grades <- matrix(c(8,15,13,14,19,15,15,4,7,3,1,4), byrow = TRUE,ncol = 3,nrow = 4,
                 dimnames = list(c("A","B","C","D-F"), c("Psychology","Biology","Other")))

grades
summary(grades)

rowsum <- apply(grades,1,sum) ; colsum<- apply(grades, 2, sum)
total <- sum(grades)

expected <- (rowsum%*%t(colsum))/total
round(expected,0)

# ((nij - Eij)^2) / Eij this formula calculates the T statistics test
sum((grades-expected)^2/ expected) # realization of statistics T
1-pchisq(12.18346,6) # p-value

z <- chisq.test(grades) ; z

chisq.test(grades,simulate.p.value = TRUE)
residuals(z) # (z$observed - z$expected)/ sqrt(z$expected)
# psychology students have relatively less A's

z$stdres # standardized residuals


# Fisher's exact test on data for right and left handed people classified by gender
handed = matrix(c(2780,3281,311,300), nrow = 2, ncol = 2, byrow = TRUE,
                dimnames = list(c("right-handed","lefhanded"),c("women","men")))
handed

fisher.test(handed)
chisq.test(handed)


# Simple Linear Regression

data <- read.table("datas/sat.txt",header = TRUE)
data
summary(data)

sat <- data[,c(1,7)] ; sat[1:2,]

satlm <- lm(total~expend, data = data)
summary(satlm) # slope is significantly negative

plot(total~expend, data = sat)
abline(satlm)

cor.test(sat$total, sat$expend) # exactly same p-value with summary of the model

# We can use data to check whether the assumptions on the errors are not totally untrue. The residuals should look normal,
# and their spread should not vary with the fitted values.

par(mfrow=c(1,2)); qqnorm(residuals(satlm)) ; plot(fitted(satlm),residuals(satlm))

# Multiple Linear Regression

data[1:3,]
plot(data[,c(1,4,7)]) ; par(mfrow=c(1,3))
for (i in c(1,4,7)) {
  hist(data[,i], main=names(data)[i])
}

sat.model <- lm(total~expend+takers,data=data) ; summary(sat.model)
confint(sat.model)

par(mfrow=c(1,2)) ; qqnorm(residuals(sat.model)) ; plot(fitted(sat.model),residuals(sat.model))


bodyfat <- read.table("datas/bodyfat.txt",header = TRUE)
bodyfat
pairs(bodyfat)

# Can we predict Fat from the other 3 variables?

bodylm <- lm(Fat~Triceps+Thigh+Midarm, data = bodyfat)
summary(bodylm)

par(mfrow=c(1,2)) ; qqnorm(residuals(bodylm)) ; plot(fitted(bodylm),residuals(bodylm))



