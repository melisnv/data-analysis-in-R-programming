data.airpollution <- read.table("datas/airpollution.txt", header = TRUE)
data.airpollution

# a
summary(data.airpollution)
# Make some graphical summaries of the data.
pairs(data.airpollution)
boxplot(data.airpollution) # There are no straight line in the scatter plot.

attach(data.airpollution)
par(mfrow=c(1,3))
qqnorm(oxidant) ; qqnorm(humidity) ; qqnorm(insolation)
hist(oxidant) ; hist(humidity) ; hist(insolation)

par(mfrow=c(1,2))
qqnorm(oxidant) ; qqnorm(temperature)
hist(oxidant) ; hist(temperature)
plot(oxidant,temperature) ; plot(oxidant,insolation)
plot(oxidant,humidity) ; plot(oxidant,wind)

# Investigate the problem of potential and influence points, and the problem of collinearity.

airpolutionlmtmp = lm(oxidant~temperature)
order(abs(residuals(airpolutionlmtmp)))

o5 = rep(0,30) ; o5[5] = 1 ; o5 # 5th outliers
airpolutionlm5 =  lm(oxidant~temperature+o5) ;
summary(airpolutionlm5)

o1 = rep(0,30) ; o1[1] = 1 ; o1 # 30th outliers
airpolutionlm1 =  lm(oxidant~temperature+o1)
summary(airpolutionlm1) 
# outliers are not significant since the coefficient for explanatory

airpolutionlmwind = lm(oxidant~wind)
order(abs(residuals(airpolutionlmwind)))
plot(residuals(airpolutionlmwind))

o8 = rep(0,30) ; o8[8] = 1 ; o8 # 17th outliers
airpolutionlm8 =  lm(oxidant~wind+o8)
summary(airpolutionlm8)

o1 = rep(0,30) ; o1[1] = 1 ; o1 # 30th outliers
airpolutionlm1 =  lm(oxidant~wind+o1)
summary(airpolutionlm1)

huberlmwind = lm(oxidant~wind)
round(cooks.distance(huberlmwind),2)
plot(1:30,cooks.distance(huberlmwind),type = 'b') # 8th point

huberlmtemp = lm(oxidant~temperature)
round(cooks.distance(huberlmtemp),2)
plot(1:30,cooks.distance(huberlmtemp),type = 'b') # 4th point

plot(temperature~wind)
pairs(data.airpollution)
round(cor(data.airpollution),2)
# checking the collinearity
# oxidant ~temperature correlation is the highest 0.76


# b
# Use the added variable plot to depict the relationship between response oxidant and predictor wind.

x=residuals(lm(wind~temperature+humidity+insolation))
y=residuals(lm(oxidant~temperature+humidity+insolation))
plot(x,y,main="Added variable plot for + wind", xlab="residual of wind",ylab="residual of oxidant")

#  What is the meaning of the slope of fitted regression for this scatter plot? 
# not so many observations. no pattern can be seen however, there is a slight aggregation around 0

# c
# Fit a linear regression model to the data. Use both the step-up and step-down methods to find the best model.
# Step-up Model

summary(lm(oxidant~wind))
summary(lm(oxidant~temperature))
summary(lm(oxidant~humidity))
summary(lm(oxidant~insolation))
# Thus, the first variable to add is wind

summary(lm(oxidant~wind+temperature)) # we can include temperature since it's significant and R-squared is higher.
summary(lm(oxidant~wind+humidity)) # we can not add humidity since it's not significant
summary(lm(oxidant~wind+insolation)) # insolation with wind is not highly significant, so we can check insolation with other factors.

summary(lm(oxidant~wind+temperature+insolation)) # we can see that insolation together with wind and temperature is not significant so do not add this.
# summary(lm(oxidant~wind+temperature) seems like correct model.
# Resulting model : oxidant = -5.20334 -0.42706 * wind + 0.52035 * temperature

# Step-down Model
summary(lm(oxidant~wind+temperature+humidity+insolation)) # with 0.65728 insolation has the highest value, so remove it
summary(lm(oxidant~wind+temperature+humidity)) # humidity has 0.131 so we'll remove it
summary(lm(oxidant~wind+temperature)) # there is no insignificant variables left.
# Resulting model : oxidant = -5.20334  + -0.42706 * wind + 0.52035 * temperature

# d
# Determine 95% confidence and prediction intervals for oxidant using the model you preferred in c) for wind=33, temperature=54, humidity=77 and insolation=21.
linearmodel.airpollution <- lm(oxidant~wind+temperature)
fitted(linearmodel.airpollution)
new.data <- data.frame(wind=33, temperature=54, humidity=77,insolation=21)
predict(linearmodel.airpollution, new.data, interval = "confidence", level= 0.95)
predict(linearmodel.airpollution, new.data, interval = "prediction", level= 0.95)

# Prediction interval is larger.