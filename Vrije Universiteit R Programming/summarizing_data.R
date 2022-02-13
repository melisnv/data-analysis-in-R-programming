setwd("C:\\Users\\Melis Nur\\Desktop\\Basic-Statistics-in-R\datas/")
getwd()

data = read.table(file = "datas/mortality.txt", header=TRUE)
head(data)
tail(data)
summary(data)
str(data)

colnames(data) <- c("ID","State","TeenBirth","Mortality")
head(data)

data$TeenBirth
data[2,3]
# Arizona state data
data[data$State == "AR",]

# plots
library(ggplot2)
ggplot(data = data, aes(x=TeenBirth, y=Mortality, colour=State)) + geom_point()

qplot(data = data, x = TeenBirth, y= Mortality, colour = State,
      alpha= I(0.6), main = "Teen Birth Rate vs Mortality in 1995")

teen_birth = data$TeenBirth ; summary(teen_birth)
mean(teen_birth)

mortality = data$Mortality ; summary(mortality)

par(mfrow=c(1,3)) ; hist(teen_birth,main = "Teen Birth Rate") ; qqnorm(teen_birth) ; boxplot(teen_birth)


# The correlation and the scatter plot between the two rates

cor(teen_birth,mortality)
plot(x=teen_birth, y = mortality, xlab="Teen Birth Rate", ylab = "Mortality")

# ---------------------------------------------------
# Distributions t-test
