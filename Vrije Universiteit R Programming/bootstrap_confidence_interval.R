data <- read.table("./datas/clouds.txt", header = TRUE)
head(data)

c1 = data[,1] # seeded
c2 = data[,2] # unseeded

T1 = mean(c1); T2 = mean(c2) ; T1 ; T2 # Different means

par(mfrow=c(1,2)) ; hist(c1,main = "Seeded Clouds") ; hist(c2,main = "Unseeded Clouds")
boxplot(c1,main="Seeded Clouds") ; boxplot(c2,main = "Unseeded Clouds")

# Bootstrap Cl in R
B = 1000
Tstar = numeric(B) ; Tstar

for (i in 1:B) {
  Xstar = sample(c1,replace = TRUE) ; # sample size : length(c1)
  Tstar [i] = mean(Xstar) ;
  
}

Tstar25 = quantile(Tstar,0.025) # T*(alpha/2)
Tstar975 = quantile(Tstar,0.975) # T*(1-alpha/2)

Tstar25 ; Tstar975
#     2.5% 
#  231.6931 
#     97.5% 
#  699.5819

sum(Tstar<Tstar25)

# Bootstrap Confidence Interval for theta:
c(2*T1 - Tstar975,2*T1-Tstar25)
# The 95% bootstrap confidence interval for the population mean of seeded clouds is [184,652]