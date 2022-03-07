data.nausea <- read.table("datas/nauseatable.txt", header = TRUE)
data.nausea

# Question a:
# 1. Discuss whether a contingency table test is appropriate here.
# Yes, it's appropriate since we have two categorical variables and independence of observations is ensured.

# 2. If yes, perform this test in order to test whether the different medicines work equally well against nausea.
n <- chisq.test(data.nausea) ; n[[3]] # 0.036
# p-value is 0.03 which is significant. Therefore, Ho: row variable and column variable are independent, is rejected.
residuals(n)
#   - From this table we can see that Chlorpromazine has less incidence of nausea, 
#   - 100mg Pentobarbital has the relatively higher nausea effect.

# Question b:
# Perform a permutation test in order to test whether the different medicines work equally well against nausea. Permute the medicine labels for this purpose.
nausea = c((rep(0, each=180)),(rep(1, each=124))) # 0 : No Incidence of Nausea, 1 : Incidence of Nausea
drug = c(factor(rep("Chlorpromazine", each=100)), factor(rep("Pentobarbital(100mg)", each=32)),factor(rep("Pentobarbital(150mg)", each=48)),
         factor(rep("Chlorpromazine", each=52)), factor(rep("Pentobarbital(100mg)", each=35)), factor(rep("Pentobarbital(150mg)", each=37)))
nausea.data <- data.frame(nausea,drug) ; nausea.data


par(mfrow=c(1,2)) ; boxplot(nausea,drug,data = nausea.data) ; plot(nausea,drug)

N <- 1000 ; Tstar <- numeric(N)
t0 <- n$statistic

mystat=function(x) sum(residuals(x)^2)
for (i in 1:N) {
  nauseaStar <- sample(nausea.data$drug)
  Tstar[i]=mystat(lm(nausea.data$nausea~nauseaStar))
}
hist(Tstar)
myt=mystat(lm(nausea.data$nausea~nausea.data$drug))

pl=sum(Tstar<myt)/N
pr=sum(Tstar>myt)/N
min(pl,pr) # 0.031
# The treatment is clearly significant.

# Use as test statistic the chi square test statistic for contingency tables, which can be extracted from the output of the command chisq.test

ctable.nausea <- table(nausea.data)
n <- chisq.test(ctable.nausea) ; n[[3]] #0.036
# This is in line with the results using the first 

# Question c:
# Compare the p-value found by the permutation test with the p-value found from the chisquare test for contingency tables.

# Both test uses bootstrap fashion to calculate p-value.
# Permutation test takes many samples (1000) and in the end estimates a similar value
# By permuting the categories of either the row or column factor in a contingency table (considered later on), one can test the null hypothesis of no dependence between these two factors.