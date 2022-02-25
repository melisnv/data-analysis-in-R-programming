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
qqnorm(residuals(additivemodel)) ; plot(fitted(additivemodel), residuals(additivemodel))

# Estimating the rate "3" by method "A"

sub.rate = subset(data, rate=="3")
sub.method = subset(sub.rate, method=="A")
mean.hemoglobin <- mean(sub.method$hemoglobin) ; mean.hemoglobin

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
