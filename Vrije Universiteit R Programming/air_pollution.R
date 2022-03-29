data.airpollution <- read.table("datas/airpollution.txt", header = TRUE)
data.airpollution

# a
summary(data.airpollution)
# Make some graphical summaries of the data.
pairs(data.airpollution)
boxplot(data.airpollution) # There are no straight line in the scatter plot.
# humidity and insolation have outliers

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

# whole model
lmwind11 = lm(oxidant~temperature+wind+humidity+insolation)
order(abs(residuals(lmwind11)))
plot(residuals(lmwind11))

o11 = rep(0,30) ; o11[11] = 1 ; o11 # 11th outliers
airpolutionlm11 =  lm(oxidant~temperature+wind+humidity+insolation+o11)
summary(airpolutionlm11)
# the coefficient for explanatory variable o11(0.0171) is insignificantly different from 0 and the outlier is significant

# partly model ( temperature and wind)

airpolutionlmtmp = lm(oxidant~temperature)
order(abs(residuals(airpolutionlmtmp)))

o4 = rep(0,30) ; o4[4] = 1 ; o4 # 4th outliers
airpolutionlm4 =  lm(oxidant~temperature+o4) ;
summary(airpolutionlm4)
# the coefficient for explanatory variable o4 is significantly different from 0 and the outlier is significant

# partly model
airpolutionlmwind = lm(oxidant~wind)
order(abs(residuals(airpolutionlmwind)))
plot(residuals(airpolutionlmwind))

o22 = rep(0,30) ; o22[22] = 1 ; o22 # 22th outliers
airpolutionlm22 =  lm(oxidant~wind+o22)
summary(airpolutionlm22)
# the coefficient for explanatory variable u22 is significantly different from 0 and the outlier is significant


# influence point

oxilm = lm(oxidant~temperature+wind+humidity+insolation)
oxi_c = round(cooks.distance(oxilm), 2) # 23rd is the most influential day with respect to the Cook's distance
plot(1:30, oxi_c, type = "b")
oxi_c #influence point: 23th - 0.83

lmwind = lm(oxidant~wind)
round(cooks.distance(lmwind),2)
plot(1:30,cooks.distance(lmwind),type = 'b') # 8th point

lmtemp = lm(oxidant~temperature)
round(cooks.distance(lmtemp),2)
plot(1:30,cooks.distance(lmtemp),type = 'b') # 4th point

lmhumid = lm(oxidant~humidity)
round(cooks.distance(lmhumid),2)
plot(1:30,cooks.distance(lmhumid),type = 'b') # 30th point

plot(temperature~wind)
pairs(data.airpollution)
round(cor(data.airpollution),2)
# checking the collinearity
# oxidant ~ temperature correlation is the highest 0.76


# b
# Use the added variable plot to depict the relationship between response oxidant and predictor wind.

x=residuals(lm(wind~temperature+humidity+insolation))
y=residuals(lm(oxidant~temperature+humidity+insolation)) 
summary(lm(oxidant~temperature+wind+humidity+insolation)) # estimate will give us the slope of all of them
plot(x,y,main="Added variable plot for + wind", xlab="residual of wind",ylab="residual of oxidant")

#  What is the meaning of the slope of fitted regression for this scatter plot? 
# wind has a negative slope while temperature has a positive and highest slope.

# c
# Fit a linear regression model to the data. Use both the step-up and step-down methods to find the best model.
# Step-up Model

summary(lm(oxidant~wind))
summary(lm(oxidant~temperature))
summary(lm(oxidant~humidity))
summary(lm(oxidant~insolation))
# Thus, the first variable to add is winds

summary(lm(oxidant~wind+temperature)) # we can include temperature since it's significant and R-squared is higher.
summary(lm(oxidant~wind+humidity)) # we can not add humidity since it's not significant
summary(lm(oxidant~wind+insolation)) # insolation with wind is not highly significant, so we can check insolation with other factors.

summary(lm(oxidant~wind+temperature+insolation)) # we can see that insolation together with wind and temperature is not significant so do not add this.
# summary(lm(oxidant~wind+temperature) seems like correct model.
# Resulting model : oxidant = -5.20334 -0.42706 * wind + 0.52035 * temperature  + error

# Step-down Model
summary(lm(oxidant~wind+temperature+humidity+insolation)) # with 0.65728 insolation has the highest value, so remove it
summary(lm(oxidant~wind+temperature+humidity)) # humidity has 0.131 so we'll remove it
summary(lm(oxidant~wind+temperature)) # there is no insignificant variables left.
# Resulting model : oxidant = -5.20334  + -0.42706 * wind + 0.52035 * temperature + error

# d
# Determine 95% confidence and prediction intervals for oxidant using the model you preferred in c) for wind=33, temperature=54, humidity=77 and insolation=21.
linearmodel.airpollution <- lm(oxidant~wind+temperature)
fitted(linearmodel.airpollution)
new.data <- data.frame(wind=33, temperature=54, humidity=77,insolation=21)
predict(linearmodel.airpollution, new.data, interval = "confidence", level= 0.95)
predict(linearmodel.airpollution, new.data, interval = "prediction", level= 0.95)

# Prediction interval is larger.