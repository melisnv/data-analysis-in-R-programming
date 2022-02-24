# Randomization in R

I <- 4 ; J <- 2 ; N <- 3
rbind(rep(1:I, each=N*I), rep(1:J, N*I), sample(1:(N*I*J)))

data <- read.table("datas/pvc.txt",header=TRUE)
data


attach(data) ; par(mfrow=c(1,2)); boxplot(psize~operator) ; boxplot(psize~resin)
# Plots the mean (or other summary) of the response for two-way combinations of factors, thereby illustrating possible interactions.
interaction.plot(operator,resin,psize) ; interaction.plot(resin,operator,psize) 

data$operator = as.factor(data$operator) ; data$resin = as.factor(data$resin) 
pvc.model <- lm(psize~operator*resin)
anova(pvc.model)
summary(pvc.model)

contrasts(data$operator) = contr.sum ; contrasts(data$resin) = contr.sum
pvc.model2 = lm(psize~operator*resin, data = data) ; summary(pvc.model2)

# Additive model
data$operator = as.factor(data$operator) ; data$resin = as.factor(data$resin)
pvc.additive.model = lm(psize~operator+resin, data=data)
anova(pvc.additive.model)

# check the normality and assumption of equal variances
qqnorm(residuals(pvc.model2)) ; plot(fitted(pvc.model2),residuals(pvc.model2))


# The following data set contains the strength of a thermoplastic composite depending on power of a laser and speed of a tape.

composite <- read.table("datas/composite.txt", header = TRUE) ; composite

attach(composite) ; anova(lm(strength~laser*tape))
# After the warning message ;
anova(lm(strength~laser+tape,data = composite))


# Randomized block design

penicillin <- read.table("datas/penicillin.txt", header = TRUE)
penicillin

xtabs(yield~treat+blend, data = penicillin)

attach(penicillin)
par(mfrow=c(1,2)) ; boxplot(yield~treat) ; boxplot(yield~blend) # A seems normally distributed

interaction.plot(treat,blend,yield) ; interaction.plot(blend,treat,yield)

penicillin.model <- lm(yield~treat+blend)
anova(penicillin.model)
# The treatment effects are not significantly different from 0. The blocks are.

summary(penicillin.model)
qqnorm(residuals(penicillin.model)) ; plot(fitted(penicillin.model),residuals(penicillin.model))


# Repeated measures
ashina <- read.table("datas/ashinal.txt", header = TRUE) 
ashina

ashina.prev <- read.table("datas/ashina.txt",header = TRUE) ; ashina.prev

ashina$id = factor(ashina$id)
ashina.model <- lm(pain~treatment+id, data = ashina) ; anova(ashina.model)

# two sample t-test
t.test(ashina.prev[,1],ashina.prev[,2], paired = TRUE)


# Friedman test
itch <- read.table("datas/itch.txt", header =TRUE, sep = ",") ; itch

duration = as.vector(as.matrix(itch[,2:8]))
id = as.factor(rep(1:10,7)) ; drug = as.factor(rep(1:7,each=10))
itchdata = data.frame(cbind(duration,id,drug))
itchdata

boxplot(duration~drug, xlab = "Drug", ylab = "Duration")
interaction.plot(drug,id,duration)

friedman.test(duration,drug,id, data = itchdata)
# Friedman test vs repeated measures
itch.model = lm(duration ~ drug + id)
anova(itch.model)

qqnorm(itch.model$residuals)
