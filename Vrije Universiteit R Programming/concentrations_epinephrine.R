# The concentrations (in nanograms per millimeter) of plasma epinephrine were measured for 10 dogs under isofluorane, halothane, and cyclopropane anesthesia, represented as three columns in data frame. We are interested in differences in the concentration for the different drugs. 
#  Is it reasonable to assume that the three columns of dogs.txt were taken from normal populations?

data <- read.table("datas/dogs.txt",header = TRUE) ; data

boxplot(data[,1],data[,2],data[,3], names = c("isofluorane","halothane","cyclopropane"))
par(mfrow=c(1,3)) ; hist(data[,1]) ; hist(data[,2]) ; hist(data[,3])
stripchart(data[,1],vertical = TRUE) ; stripchart(data[,2],vertical = TRUE) ; stripchart(data[,3],vertical = TRUE)
# Column cyclopropane seems normal population, however column isofluorane and halothane seem like do not represent a normal population

shapiro.test(data[,1]) ; shapiro.test(data[,2]) ; shapiro.test(data[,3])


# Investigate whether the columns isofluorane and halothane are correlated. Apply relevant tests to verify whether the distributions of these columns are different. Is a permutation test applicable? 

# Since these two columns not normally distributed, we can use Spearman's correlation test.
attach(data)
plot(isofluorane~halothane) # Based on this plot, we expect independence between isofluorane and halothane.
qqnorm(isofluorane, main = "QQ Plot isofluorane") ; qqnorm(halothane, main = "QQ Plot halothane") #The histogram of outcome does not appear normally distributed.

cor.test(isofluorane,halothane, method = "spearman") # The value of ρ=0.22 is positive close to zero. Closer to zero indicates little correlation between X and Y rankings.

# permutation test
func <- function(x,y) {mean(x-y)}
B <- 1000 ; Tstar <- numeric(B)
for (i in 1:B) {
  data.star <- t(apply(cbind(data[,1],data[,2]),1,sample))
  Tstar[i] <- func(data.star[,1], data.star[,2])
  
}

res <- func(data[,1], data[,2]) ; res
hist(Tstar)
pl <- sum(Tstar<res)/B ; pr <- sum(Tstar>res)/B ; p <- 2*min(pl,pr)
pl ; pr ; p

# Conduct a one-way ANOVA to determine whether the type of drug has an effect on the concentration of plasma epinephrine. Give the estimated concentrations for each of the three anesthesia drugs. 
boxplot(data) ; stripchart(data, vertical = TRUE)

drugframe <- data.frame(plasma = as.vector(as.matrix(data)), drugs = factor(rep(1:3, each=10))) ; drugframe
is.factor(drugframe$drugs) ; is.numeric(drugframe$drugs) # TRUE - FALSE

drugs.model = lm(plasma~drugs, data = drugframe) ; drugs.model
anova(drugs.model)
# Ha is rejected since p-value (0.01) is less than 0.05

summary(drugs.model)
confint(drugs.model)

par(mfrow=c(1,2)) ; qqnorm(residuals(drugs.model))
plot(fitted(drugs.model), residuals(drugs.model))

# Does the Kruskal-Wallis test arrive at the same conclusion about the effect of drug as the test in c)? Explain possible differences between conclusions of the Kruskal-Wallis and ANOVA tests. 
attach(drugframe) ; kruskal.test(plasma, drugs)
# p-value = 0.05948, hence Ho is not rejected since it is greater than 0.05
# In my result ANOVA and Kruskal-Wallis yield a slight difference. ANOVA assumes normality whereas Kruskal-Wallis does not.
