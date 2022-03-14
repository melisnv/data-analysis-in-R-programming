awards.data <- read.table("datas/awards.txt", header = TRUE)
awards.data

pairs(awards.data)

# a
awardglm <- glm(num_awards ~ prog, family = poisson, data = awards.data)
summary(awardglm) # the type of program does not influence the number of awards

awards.data$prog = as.factor(awards.data$prog)
awardglm <- glm(num_awards ~ prog, family = poisson, data = awards.data)
summary(awardglm) # not significant 0.141
# program2(0.00106) is significant with program3(0.07199 ) is not significant
# program2 has a positive estimated value and significant p-value, therefore program2 is efficient.


# b
kruskal.test(num_awards ~ prog, data= awards.data) # 0.00462


# c
awardglm2 <- glm(num_awards ~ prog + math, family = poisson, data = awards.data)
summary(awardglm2)


awardglm.interaction <- glm(num_awards ~ prog * math, family = poisson, data = awards.data)
summary(awardglm.interaction)


# Estimate the number of awards for the vocational program and math score 55.
new.data = data.frame(prog="1",math=55)
predict(awardglm2,new.data,type = "response") # 0.6671683 
predict(awardglm.interaction,new.data,type = "response") # 0.6323158 
