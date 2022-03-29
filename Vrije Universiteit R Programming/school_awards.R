awards.data <- read.table("datas/awards.txt", header = TRUE)
awards.data

pairs(awards.data)

# a
# Investigate whether the type of program influences the number of awards by performing a Poisson regression, without taking variable math into account. Estimate the numbers of awards for all the three types of program. Which program type is the best for the number of awards for this model? 
awardglm <- glm(num_awards ~ prog, family = poisson, data = awards.data)
summary(awardglm) # the type of program does not influence the number of awards

awards.data$prog = as.factor(awards.data$prog)
awardglm <- glm(num_awards ~ prog, family = poisson, data = awards.data)
summary(awardglm) # not significant 0.141
# program2(0.00106) is significant with program3(0.07199 ) is not significant
# program2 has a positive estimated value and significant p-value, therefore program2 is efficient.


# b
# For the situation in a), can the Kruskall-Wallis test also be used? If yes, apply the test and comment on the results; if no, explain why this test cannot be used. 
kruskal.test(num_awards ~ prog, data= awards.data) # 0.00462

# kruskall test is valid because assumptions are met. 

# c
# Now include predictor math into analysis and investigate the influence of the explanatory variables prog and math (and their interaction) on the numbers of awards.
awardglm2 <- glm(num_awards ~ prog + math, family = poisson, data = awards.data)
summary(awardglm2)
# prog2 : 0.0440, prog3 : 0.0232, math : 1.80e-05

awardglm.interaction <- glm(num_awards ~ prog * math, family = poisson, data = awards.data)
summary(awardglm.interaction) # their interaction is not significant

#Which program type is the best for the number of awards? Comments on your findings. 
# prog2

# Estimate the number of awards for the vocational program and math score 55.
new.data = data.frame(prog="1",math=55)
predict(awardglm2,new.data,type = "response") # 0.6671683 
predict(awardglm.interaction,new.data,type = "response") # 0.6323158 

fitted(awardglm.interaction)
