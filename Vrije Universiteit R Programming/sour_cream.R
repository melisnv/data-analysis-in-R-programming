# Analyze the data in a three-way experiment without interactions with acidity as response and starter, batch and position as factors. 
# By using summary command, can you tell whether there is a significant difference between the effects of starter 1 and starter 2 on acidity?
library(lme4)

creamdata <- read.table("datas/cream.txt", header = TRUE)
creamdata

# checking normality
par(mfrow=c(1,3))
attach(creamdata)
boxplot(acidity~batch)
boxplot(acidity~position)
boxplot(acidity~starter)

# We construct a boxplot to get an understanding of the factors' primary effects. 
# Interactions are hidden from sight. 
# Another reason we constructed boxplots is to check whether there are significant outliers or not. 
#The reason we do this is that outliers can have a negative effect on the three-way ANOVA, reducing the accuracy of our results.

par(mfrow=c(1,4))
attach(creamdata)
interaction.plot(batch,position,acidity)
interaction.plot(position,batch,acidity)
interaction.plot(batch,starter,acidity)
interaction.plot(starter,position,acidity)

# We used interaction plots to understand the behavior of one variable depending on the value of another variable and to observe interactions clearly.
# Some nonparallel curves can be seen, indicating interactions.

shapirotest.pvalue <- round(shapiro.test(acidity)$p.value, digits = 3)
shapiro.test(batch)
shapiro.test(starter)
shapiro.test(position)

# For any combination of the factors, our dependent variable should be approximately regularly distributed. 
# For doing a three-way ANOVA, the data just has to be roughly normal because it is "robust" to breaches of normality, meaning that it can be slightly violated and still produce accurate findings. 
# As a result, we'll use the Shapiro-Wilk normality test to determine whether or not the data is normal.
# Since the p-value value is not less than .05, we can assume the dataset comes from a population that is normally distributed.

btest.batch <- round(bartlett.test(acidity ~ batch, data=creamdata)$p.value, digits=3)
btest.starter <- round(bartlett.test(acidity ~ starter, data=creamdata)$p.value, digits=3)
btest.position <- round(bartlett.test(acidity ~ position, data=creamdata)$p.value, digits=3)

# For each combination of the groups containing the three independent variables, there must be homogeneity of variances. As a result, we use the Bartlett test of homogeneity of variances to evaluate this assumption.
# With the obtained p-values from the test, we can see that homogeneity of variances was confirmed. Therefore, we can start to apply three-way ANOVA.

# 3-way
creamdata$starter=factor(creamdata$starter) ; creamdata$position=factor(creamdata$position)
creamdata$batch=factor(creamdata$batch)
creamModel=lm(acidity~batch+position+starter,data=creamdata)

anova.of.model<- round(anova(creamModel), digits = 3)
anova.of.model
#anova.of.model$`Pr(>F)`[1]

summary.of.model <- summary(creamModel)
round(summary.of.model$coefficients, digits = 3)
# Using the summary command, we can observe that the effects of starter1 and starter2 on acidity are not significantly different.

#b

# From the previous ANOVA result, we obtained that position factor is insignificant. Therefore we remove position from our model and apply ANOVA with the remaining significant factors.

#creamModel2=lm(acidity~starter*batch+position,data=creamdata)
creamModel2=lm(acidity~batch+starter,data=creamdata)
anova.of.model2<- round(anova(creamModel2), digits = 3)
anova.of.model2

summary.of.model2 <- summary(creamModel2)

starter1 <- summary.of.model2$coefficients[1:1,]
starters <- summary.of.model2$coefficients[6:9,]

starter1 <- c(round(summary.of.model2$coefficients[1:1,], digits = 3))
starter2 <- c(round(summary.of.model2$coefficients[6:6,], digits = 3))
starter3 <- c(round(summary.of.model2$coefficients[7:7,], digits = 3))
starter4 <- c(round(summary.of.model2$coefficients[8:8,], digits = 3))
starter5 <- c(round(summary.of.model2$coefficients[9:9,], digits = 3))

dataframe.starters <- data.frame(starter1, starter2, starter3, starter4, starter5)
final.df.starters <- as.data.frame(t(dataframe.starters))
final.df.starters

# We fitted a linear model to predict acidity with batch and starter. The model explains a statistically significant and substantial proportion of variance.
# The effect of starter2,starter3, and starter5 is statistically non-significant and negative whereas the effect of starter4 is statistically significant and positive.

par(mfrow=c(1,2)) ; qqnorm(residuals(creamModel2)); plot(fitted(creamModel2),residuals(creamModel2))

# Finally, we'll search for normality of errors by plotting the residuals, which are data corrected for various population means and should appear normal. From observed plots, we can say that the residuals QQ-plot on the left appear to be normal. 
# The spread in the residuals does not fluctuate in a predictable way as a function of any variable, particularly the fitted values. The fitted values against the residuals graph on the right appear to be normal, with no noticeable trend. The proposed fixed effects model is reported to be a suitable fit.

# c
par(mfrow=c(1,2))
boxplot(acidity~starter,xlab="acidity",ylab="starter")
interaction.plot(starter,batch,acidity)

friedman.test.pvalue <- round(friedman.test(acidity,starter,batch,data=creamdata)$p.value, digits= 3)
friedman.test.pvalue

# The test result reinforces what we have already discovered previously, the null hypothesis is rejected.

# d
creamlmer=lmer(acidity~starter+(1|batch), data=creamdata, REML=FALSE)
anova.of.lmer <- round(anova(creamlmer), digits = 3)
anova.of.lmer

summary.of.lmer <-summary(creamlmer)
round(summary.of.lmer$coefficients, digits = 3)
round(summary.of.model2$coefficients, digits = 3)

# In order to implement the mixed-effects models, we use the lme4 library.  The function lmer gives the correct implementation of the crossover design, with the individuals as “random effects”. 
# We fitted a linear mixed model to predict acidity with starter. The model included position and batch as random effects. The model's explanatory power related to the fixed effects alone is 0.03. The effect of starter is statistically non-significant and positive. 
# The estimated starter and batch effects under Fixed effects are slightly different from those in the previously obtained fixed-effect result.

