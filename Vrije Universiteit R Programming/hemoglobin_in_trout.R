# Hemoglobin is measured (g/100 ml.) in the blood of brown trout after 35 days of treatment with four rates of sulfamerazine: the daily rates of 0, 5, 10 and 15 g of sulfamerazine per 100 pounds of fish, denoted as rates 1, 2, 3 and 4, respectively. 
# (Beware that the levels of the factor rate are coded by numbers.) Two methods (denoted as A and B) of administering the sulfamerazine were used.

data <- read.table("datas/hemoglobin.txt", header = TRUE)
data

# Present an R-code for the randomization process to distribute 80 fishes over all combinations of levels of factors rate and method.
I <- 4 ; J <- 2 ; N <- 10 # I : level 
rbind(rep(1:I,each=N*J), rep(1:J,N*I),sample(1:(N*I*J)))


# Perform the two-way ANOVA to test for effects of factors rate, method and their interaction on the response variable hemoglobin. Comment on your findings. 
attach(data)
par(mfrow=c(1,2)) ; boxplot(hemoglobin~rate) ; boxplot(hemoglobin~method) # Interactions are not visible

interaction.plot(rate,method,hemoglobin) ; interaction.plot(method,rate,hemoglobin)

data$rate = as.factor(data$rate) ; data$method = as.factor(data$method)
datamodel = lm(hemoglobin~rate*method, data = data)
anova(datamodel)

summary(datamodel)

# Which of the two factors has the greatest influence? Is this a good question? Consider the additive model. Which combination of rate and method yield the highest hemoglobin? Estimate the mean hemoglobin value for rate 3 by using method A.
# What rate leads to the highest mean hemoglobin? 

# Additive model
data$rate = as.factor(data$rate) ; data$method = as.factor(data$method)
additivemodel = lm(hemoglobin~rate+method, data = data)
anova(additivemodel)

summary(additivemodel)

# Checking the normality
qqnorm(residuals(additivemodel)) ; plot(fitted(additivemodel), residuals(additivemodel)) # normal

# Estimating the rate "3" by method "A"
sub.rate1 = subset(data, rate=="1")
sub.rate2 = subset(data, rate=="2")
sub.rate3 = subset(data, rate=="3")
sub.rate4 = subset(data, rate=="4")

sub1.methodA = subset(sub.rate1, method=="A")
sub2.methodA = subset(sub.rate2, method=="A")
sub3.methodA = subset(sub.rate3, method=="A")
sub4.methodA = subset(sub.rate4, method=="A")

sub1.methodB = subset(sub.rate1, method=="B")
sub2.methodB = subset(sub.rate2, method=="B")
sub3.methodB = subset(sub.rate3, method=="B")
sub4.methodB = subset(sub.rate4, method=="B")

mean.hemoglobin1A <- mean(sub1.methodA$hemoglobin) ; mean.hemoglobin1A
mean.hemoglobin2A <- mean(sub2.methodA$hemoglobin) ; mean.hemoglobin2A
mean.hemoglobin3A <- mean(sub3.methodA$hemoglobin) ; mean.hemoglobin3A
mean.hemoglobin4A <- mean(sub3.methodA$hemoglobin) ; mean.hemoglobin4A

mean.hemoglobin1B <- mean(sub1.methodB$hemoglobin) ; mean.hemoglobin1B
mean.hemoglobin2B <- mean(sub2.methodB$hemoglobin) ; mean.hemoglobin2B
mean.hemoglobin3B <- mean(sub3.methodB$hemoglobin) ; mean.hemoglobin3B
mean.hemoglobin4B <- mean(sub3.methodB$hemoglobin) ; mean.hemoglobin4B

# Test the null hypothesis that the hemoglobin is the same for all rates by a one-way ANOVA test, ignoring the variable method. Is it right/wrong or useful/not useful to perform this test on this data set?
hemoglobin.dataframe <- data.frame(hemoglobin = as.vector(as.matrix(data$hemoglobin)), rate = factor(rep(1:4,each=20)))
hemoglobin.dataframe  

is.factor(hemoglobin.dataframe$rate) ; is.numeric(hemoglobin.dataframe$rate)

hemoglobin.model = lm(hemoglobin~rate, data = hemoglobin.dataframe)  
anova(hemoglobin.model)   # one-way ANOVA assumes normality

summary(hemoglobin.model)
confint(hemoglobin.model)
qqnorm(residuals(hemoglobin.model)) ; plot(fitted(hemoglobin.model), residuals(hemoglobin.model)) # according to plots, there is no huge difference 

# A one-way ANOVA only involves one factor or independent variable, whereas there are two independent variables in a two-way ANOVA
# In a one-way ANOVA, the one factor or independent variable analyzed has three or more categorical groups. A two-way ANOVA instead compares multiple groups of two factors. 
