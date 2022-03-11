esophagus = read.table("datas/esoph.txt",header = TRUE)
head(esophagus)

hist(esophagus[,1],main = "age") # age distribution

tot = xtabs(~alc+tob, data = esophagus) ; tot
tot.c = xtabs(cancer~alc+tob, data = esophagus)
round(tot.c/tot,2) # percentage of individuals with cancer for every comb. of alchocol and tobacco use

totage = xtabs(~age,data=esophagus)
barplot(xtabs(cancer~age, data = esophagus)/totage)

esophagus$age2 = esophagus$age^2
esophaguslm = glm(cancer~age+age2+alc+tob, data = esophagus, family = binomial)
summary(esophaguslm)


esophagus$age = as.factor(esophagus$age) 
esophagus$alc = as.factor(esophagus$alc)
esophagus$tob = as.factor(esophagus$tob)

glm2 = glm(cancer~age+alc+tob, data=esophagus, family = binomial)
summary(glm2)

new.data = data.frame(age="70",alc="20",tob="35")
fitted(glm2)

predict(glm2, new.data, type = "response")
plot(c(0,coef(glm2)[2:6]),type = "l")

drop1(glm2, test = "Chisq")

# Aggregated data format
esophshort = read.table("datas/esophshort.txt", header = TRUE)
esophshort$age2 = esophshort$age^2
head(esophshort)

shorthglm = glm(cbind(ncases,ncontrols)~age+age2+alc+tob, data = esophshort, family = binomial)
summary(shorthglm)

glm4 = glm(cancer~age*alc, data = esophagus, family = binomial)
anova(glm4, test = "Chisq")

