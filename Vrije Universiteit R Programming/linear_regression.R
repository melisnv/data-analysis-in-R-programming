data <- read.table("datas/bodyfat.txt",header = TRUE)
data

# STEP UP APPROACH
# First step up
summary(lm(data$Fat~data$Triceps))
summary(lm(data$Fat~data$Thigh))
summary(lm(data$Fat~data$Midarm))

# Second step up
summary(lm(data$Fat~data$Thigh+data$Midarm))
summary(lm(data$Fat~data$Thigh+data$Triceps))
# Our model only include Thigh since it's significant

# STEP DOWN APPROACH
summary(lm(data$Fat~data$Thigh+data$Triceps+data$Midarm)) # None of the variables are significant, and the biggest p-value belongs to Thigh, so remove it!

# Second step up
summary(lm(data$Fat~data$Triceps+data$Midarm)) # there are no insignificant anymore ,therefore can not remove any factor

bodyfatlm <- lm(Fat~Triceps+Thigh+Midarm, data = data)
summary(bodyfatlm)
fitted(bodyfatlm)

# Prediction intervals for the body fat data
newxdata <- data.frame(Triceps=24.5,Thigh=51.3,Midarm=28.7)
predict(bodyfatlm,newxdata,interval = "prediction")
predict(bodyfatlm,newxdata,interval = "confidence")

# Diagnostic plots
pairs(data)

bodyfatlm = lm(data$Fat~data$Thigh)
plot(residuals(bodyfatlm),data$Thigh)

attach(data)
x = residuals(lm(Thigh~Midarm+Triceps))
y = residuals(lm(Fat~Midarm+Triceps))
plot(x,y,main="Added variable plot for + Thigh", xlab="residual of Thigh",ylab="residual of Fat")
# The slope in this plot reflects the regression coefficients.

# Scatter plot of the residuals against each Xk, which are not in the model
plot(residuals(bodyfatlm),Triceps)
plot(residuals(bodyfatlm),Midarm)

plot(residuals(bodyfatlm),Fat) # Y
plot(residuals(bodyfatlm),fitted(bodyfatlm)) # Y^


qqnorm(residuals(bodyfatlm))

forbes = read.table("datas/forbes.txt",header=TRUE)
forbes

x = forbes[,2] ; y=forbes[,3]
x ; y
forbeslm = lm(y~x)

plot(forbeslm)

order(abs(residuals(forbeslm)))
# To check whether the outlier is significant or no
u11 = rep(0,16) ; u11[11] = 1 ; u11 # creates a dumm yvector and include it in the model
forbestlm.outlier = lm(y~x+u11) ; summary(forbestlm.outlier)
# outlier is significant since 2.03e-06 < 0.05

round(cor(data),2)
pairs(data) # Triceps and Thigh is collinear



# VIF-values
bodyfatlm = lm(Fat~Thigh+Triceps+Midarm, data = data)
library(car) ; vif(bodyfatlm)

bodyfatlm2 = lm(Fat~Triceps+Midarm, data = data) ; vif(bodyfatlm2)
bodyfatlm3 = lm(Fat~Thigh, data = data) ; vif(bodyfatlm3)
