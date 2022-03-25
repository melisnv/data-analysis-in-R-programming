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

iso = fruitflies[fruitflies['activity']=='isolated',]
low = fruitflies[fruitflies['activity']=='low',]
high = fruitflies[fruitflies['activity']=='high',]
# low2 = fruitflies[fruitflies$activity=='low',]


mean(low$loglongevity) #  3.999836
mean(low$longevity) #  3.999836
exp(3.999836) # 54.5892

iso ; low ; high
# 4.119349 3.999836 3.602124

# negative relationship


#estimated.longevities <- c(0,0,0)
#for (i in 1:3) {
#  base <- summary(fruitflies.lm1)$coeff[i]
#  estimated.longevities[i] <- summary(fruitflies.lm1)$coeff[i]
#}
#estimated.longevities

par(mfrow=c(1,2))
qqnorm(residuals(fruitflies.lm1)) ; plot(residuals(fruitflies.lm1),fitted(fruitflies.lm1))

# b
# Investigate whether sexual activity influences longevity by performing a statistical test, now including thorax length as an explanatory variable into the analysis. 

fruitflies.lm2 <- lm(loglongevity~thorax+activity, data = fruitflies)
anova(fruitflies.lm2)
summary(fruitflies.lm2) # thorax and activity significant

qqnorm(residuals(fruitflies.lm2)) ; plot(residuals(fruitflies.lm2),fitted(fruitflies.lm2))


drop1(fruitflies.lm2,test="F") # both thorax and activity are significant, but with throax activity's significance increased

# Does sexual activity increase or decrease longevity? What are the estimated longevities for the three groups, for flies with the minimal and maximal thorax lengths?
# Since β = 2.97899 > 0, the higher the throax, number of days until death increases. Isolated activity has the highest effect. Factor activity is significant.

thr_actlm = lm(loglongevity~thorax+activity, data = fruitflies)

iso_thr_max = data.frame(activity="isolated",thorax = max(fruitflies$thorax))
predict(thr_actlm,iso_thr_max,type = "response") #4.429164
exp(4.429164)# 83.86128 ~ 83/84 days

iso_thr_min = data.frame(activity="isolated",thorax = min(fruitflies$thorax))
predict(thr_actlm,iso_thr_min,type = "response") #3.535467
exp(3.535467)# 34.31103 ~ 34 days

low_thr_max = data.frame(activity="low",thorax = max(fruitflies$thorax))
predict(thr_actlm,low_thr_max,type = "response") #4.304884
exp(4.304884)#74.06062 ~ 74 days

low_thr_min = data.frame(activity="low",thorax = min(fruitflies$thorax))
predict(thr_actlm,low_thr_min,type = "response") #3.411188
exp(3.411188)#30.30122 ~ 30 days

high_thr_max = data.frame(activity="high",thorax = max(fruitflies$thorax))
predict(thr_actlm,high_thr_max,type = "response") #4.019183
exp(4.019183)#55.65562 ~ 55/56 days

high_thr_min = data.frame(activity="high",thorax = min(fruitflies$thorax))
predict(thr_actlm,high_thr_min,type = "response") #3.125486
exp(3.125486)#22.77096 ~ 22/23 days

# *********************************************************************************
highData = fruitflies[which(fruitflies['activity']=='high'),]
highData['loglongevity'][which.max(highData$thorax),] # MAX 3.78419
highData['loglongevity'][which.min(highData$thorax),] # MIN 2.772589

lowData = fruitflies[which(fruitflies['activity']=='low'),]
lowData['loglongevity'][which.max(lowData$thorax),] # MAX 3.871201
lowData['loglongevity'][which.min(lowData$thorax),] # MIN 3.044522

isolatedData = fruitflies[which(fruitflies['activity']=='isolated'),]
isolatedData['loglongevity'][which.max(isolatedData$thorax),] # MAX 4.317488
isolatedData['loglongevity'][which.min(isolatedData$thorax),] # MIN 3.688879

# ALTERNATIVELY for all the groups not individually
fruitflies['loglongevity'][which.max(fruitflies$thorax),] # 4.317488


# *********************************************************************************

# c
# How does thorax length influence longevity? Investigate graphically and by using an appropriate test whether this dependence is similar under all three conditions of sexual activity. 
plot(loglongevity~thorax, pch=unclass(activity))
for (i in fruitflies$activity) {
  abline(lm(loglongevity~thorax, data=fruitflies[fruitflies$activity==i,]))
}

plot(thorax,loglongevity)
boxplot(loglongevity~activity)

boxplot(loglongevity~thorax,data = fruitflies,main = "Longetivity by Thorax",
        xlab = "Thorax",ylab = "Log of Longetivity")

fruitflies.lm3 <- lm(loglongevity~thorax, data = fruitflies)
anova(fruitflies.lm3) # 3.151e-13
summary(fruitflies.lm3) # 3.15e-1 significant

fruitflies.lm4 <- lm(loglongevity~activity+thorax, data = fruitflies)
anova(fruitflies.lm4) # 1.139e-14
summary(fruitflies.lm4) # 1.14e-14

deneme.lm4 <- lm(loglongevity~thorax+activity, data = fruitflies)
anova(deneme.lm4) # 
summary(deneme.lm4) # 

interaction.lm <- lm(loglongevity~activity*thorax, data = fruitflies)
anova(interaction.lm)
summary(interaction.lm)
# interaction term is not significant.

# d
# Which of the two analyses, without or with thorax length, do you prefer? Is one of the analyses wrong? 

model <- aov(thorax ~ activity, data = fruitflies)
summary(model)
# The p-value is 0.163 that is greater than 0.05, so the covariate thorax and the treatment activity are independent to each other.
# Both analysis are correct and useful.

# e
# Perform the ancova analysis with the number of days as the response, rather than its logarithm. Was it wise to use the logarithm as response?

plot(longevity~thorax, pch=as.character(activity))
par(mfrow=c(1,2))
boxplot(loglongevity~activity,data = fruitflies,main = "log(Longetivity) by Activity",
        xlab = "Activity",ylab = "Log of Longetivity")
boxplot(longevity~activity,data = fruitflies,main = "Longetivity by Activity",
        xlab = "Activity",ylab = "Longetivity")

longevity.lm <- lm(longevity~thorax+activity, data = fruitflies)
anova(longevity.lm)
summary(longevity.lm)
drop1(longevity.lm, test = "F")

longevity.lm1 <- lm(longevity~activity*thorax, data = fruitflies)
anova(longevity.lm1)
summary(longevity.lm1)

moddy1=lm(longevity~activity+ thorax)
moddy2=lm(longevity~thorax+activity)

anova(moddy1) # thorax 2.62e-12
summary(moddy2) # thorax 2.62e-12

par(mfrow=c(1,2))
qqnorm(residuals(longevity.lm)) ; plot(residuals(longevity.lm),fitted(longevity.lm))

# Data is already normal, so no need to do the log

