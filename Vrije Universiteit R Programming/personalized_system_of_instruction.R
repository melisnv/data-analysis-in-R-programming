psi.data <- read.table("datas/psi.txt",header=TRUE)
psi.data

summary(psi.data)
hist(psi.data[,3],main="GPA Distribution",xlab = "gpa")
boxplot(psi.data)

table1 = xtabs(~passed + psi, data = psi.data) ; table1
totgpa = xtabs(~gpa, data=psi.data)
barplot(xtabs(passed~gpa, data = psi.data)/totgpa)

# Adjust the data
psi.data$psi = as.factor(psi.data$psi)
psi.data$trunc.gpa = as.factor(trunc(psi.data$gpa))

# a  
# Fit a logistic regression model with both explanatory variables, perform relevant tests. Does psi work? 
psiglm <- glm(passed ~ psi + gpa, data = psi.data, family = binomial)
summary(psiglm)
# Both psi and gpa are statistically significant. 
# For every one unit change in psi, the log odds of passed (versus non-passed) increases by 2.338.
# For a one unit increase in gpa, the log odds of being passed increases by 3.063.
confint(psiglm)
fitted(psiglm)

anova(psiglm, test = "Chisq")
drop1(psiglm, test = "Chisq")

par(mfrow=c(1,2))
qqnorm(residuals(psiglm)) ; plot(residuals(psiglm),fitted(psiglm))

# b
# Estimate the probability that a student with a gpa equal to 3 who receives psi passes the assignment.  
new.data <- data.frame(gpa=3, psi="1")
predict(psiglm, new.data, type = "response" ) # the predicted probability of being passed is 0.4815864

# Estimate the same probability for a student who does not receive psi.
new.data2 <- data.frame(gpa=3, psi="0")
predict(psiglm, new.data2, type = "response" ) # the predicted probability of being passed is 0.08230274 

plot(c(0,coef(psiglm)[2:3]),type = "l") # coefficients for different gpa categories

# c
# todo : ******************************************************************************
# Estimate the relative change in odds of passing the assignment rendered by instructing students with psi rather than the standard method (for an arbitrary student). What is the interpretation of this number? Is it dependent on gpa?

interaction.psi <- glm(passed ~ trunc.gpa + psi, data = psi.data, family = binomial)
summary(interaction.psi)
fitted(interaction.psi)

# group(gpa3,psi1)
odds1 <- exp((-2.940)+(1.639)+(2.091)) ; odds1 # 2.203396

# group(gpa4,psi1)
odds2 <- exp((-2.940)+(19.720)+(2.091)) ; odds2 # 156881296

# group(gpa3,psi0)
odds3 <- exp((-2.940)+(1.639)+(0)) ; odds3 # 0.2722594

# group(gpa4,psi0)
odds4 <- exp((-2.940)+(19.720)+(0)) ; odds4 # 19384804

# d
# todo : ******************************************************************************
# Propose and perform an alternative method of analysis based on contingency tables. Compare its results to the results of the first approach. 

ctable1.psi = table(psi.data$trunc.gpa,psi.data$passed)
ctable2.psi = table(psi.data$psi,psi.data$passed)
chisq.test(ctable1.psi) # odds ratio : 0.02058 (Ho : true odds ratio is equal to 1)
chisq.test(ctable2.psi) # odds ratio : 0.04376 (Ho : true odds ratio is equal to 1)

fisher.test(ctable1.psi) # 0.02333
fisher.test(ctable2.psi) # 0.0265

# e
# todo : ******************************************************************************
# Given the way the experiment was conducted, is this second approach wrong? Name both an advantage and a disadvantage of the two approaches, relative to each other.



