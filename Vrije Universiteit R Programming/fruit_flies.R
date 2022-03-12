fruitflies <- read.table("datas/fruitflies.txt",header=TRUE)
fruitflies
summary(fruitflies)

# Add a column loglongevity to the data-frame, containing the logarithm of the number of days until death.
fruitflies$loglongevity <- log(as.numeric(fruitflies$longevity)) ; fruitflies

# a
# Make an informative plot of the data.
attach(fruitflies)
plot(loglongevity~thorax, pch=as.character(activity))

boxplot(loglongevity~activity,data = fruitflies,main = "log(Longetivity) by Activity",
        xlab = "Activity",ylab = "Log of Longetivity")

# Investigate whether sexual activity influences longevity by performing a statistical test, without taking the thorax length into account.
fruitflies$activity = as.factor(fruitflies$activity)

fruitflies.lm1 <- lm(loglongevity~activity, data = fruitflies)
anova(fruitflies.lm1)
summary(fruitflies.lm1) # activity is significant

# What are the estimated longevities for the three conditions?
estimated.longevities <- c(0,0,0)
for (i in 1:3) {
  estimated.longevities[i] <- summary(fruitflies.lm1)$coeff[i]
}
estimated.longevities

par(mfrow=c(1,2))
qqnorm(residuals(fruitflies.lm1)) ; plot(residuals(fruitflies.lm1),fitted(fruitflies.lm1))

# b
# Investigate whether sexual activity influences longevity by performing a statistical test, now including thorax length as an explanatory variable into the analysis. 

model <- aov(thorax ~ activity, data = fruitflies)
summary(model)
# The p-value is 0.163 that is greater than 0.05, so the covariate thorax and the treatment activity are independent to each other.


fruitflies.lm2 <- lm(loglongevity~thorax+activity, data = fruitflies)
anova(fruitflies.lm2)
summary(fruitflies.lm2) # thorax and activity significant

drop1(fruitflies.lm2,test="F") # both thorax and activity are significant, but with throax activity's significance increased

# Does sexual activity increase or decrease longevity? What are the estimated longevities for the three groups, for flies with the minimal and maximal thorax lengths?
# Since β = 2.97899 > 0, the higher the throax, number of days until death increases. Isolated activity has the highest effect. Factor activity is significant.
estimated.longevities.with.thorax <- c(0,0,0)
for (i in 1:4) {
  estimated.longevities.with.thorax[i] <- summary(fruitflies.lm2)$coeff[i]
}
estimated.longevities.with.thorax

qqnorm(residuals(fruitflies.lm2)) ; plot(residuals(fruitflies.lm2),fitted(fruitflies.lm2))

# c
# How does thorax length influence longevity? Investigate graphically and by using an appropriate test whether this dependence is similar under all three conditions of sexual activity. 
plot(loglongevity~thorax, pch=unclass(activity))
for (i in fruitflies$activity) {
  abline(lm(loglongevity~thorax, data=fruitflies[fruitflies$activity==i,]))
}

boxplot(loglongevity~thorax,data = fruitflies,main = "Longetivity by Thorax",
        xlab = "Thorax",ylab = "Log of Longetivity")

fruitflies.lm3 <- lm(loglongevity~thorax, data = fruitflies)
anova(fruitflies.lm3)
summary(fruitflies.lm3)

fruitflies.lm4 <- lm(loglongevity~activity+thorax, data = fruitflies)
anova(fruitflies.lm4)
summary(fruitflies.lm4)

interaction.lm <- lm(loglongevity~activity*thorax, data = fruitflies)
anova(interaction.lm)
summary(interaction.lm)
# interaction term is not significant.

# d
# todo : ****************************************
# Which of the two analyses, without or with thorax length, do you prefer? Is one of the analyses wrong? 


# e
# todo : ****************************************
# Perform the ancova analysis with the number of days as the response, rather than its logarithm. Was it wise to use the logarithm as response?

plot(longevity~thorax, pch=as.character(activity))
par(mfrow=c(1,2))
boxplot(loglongevity~activity,data = fruitflies,main = "log(Longetivity) by Activity",
        xlab = "Activity",ylab = "Log of Longetivity")
boxplot(longevity~activity,data = fruitflies,main = "Longetivity by Activity",
        xlab = "Activity",ylab = "Longetivity")

longevity.lm <- lm(longevity~activity+thorax, data = fruitflies)
anova(longevity.lm)
summary(longevity.lm)

longevity.lm1 <- lm(longevity~activity*thorax, data = fruitflies)
anova(longevity.lm1)
summary(longevity.lm1)
