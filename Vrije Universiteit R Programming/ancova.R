fiber <- read.table("datas/fiber.txt", header = TRUE)
fiber

attach(fiber)
plot(strength~thickness, pch=as.character(type)) # informative plot

fiber$type = as.factor(fiber$type)
anova(lm(strength~type, data = fiber)) # checking the effect of "type" by not adding the "thickness"

fiber1 = lm(strength~thickness+type, data = fiber) # type should be second
anova(fiber1)

# drop1 performs the tests for both models at once.
drop1(fiber1, test = "F")
summary(fiber1)

par(mfrow=c(1,2)) ; qqnorm(residuals(fiber1)) ; plot(fitted(fiber1),residuals(fiber1)) # not enough observations

plot(strength~thickness, pch=unclass(type))
for (i in 1:3) {
  abline(lm(strength~thickness,data = fiber[fiber$type==i,]))
}    

fiber3 = lm(strength~type*thickness, data = fiber)
anova(fiber3) # the hypthesis Ho : β1 = β2 is not rejected
summary(fiber3)

pvc <- read.table("datas/pvc.txt",header=TRUE)
pvc

pvc$operator = as.factor(pvc$operator) ; pvc$resin = as.factor(pvc$resin)
pvc.anova = lm(psize~operator*resin, data = pvc)
summary(pvc.anova)

library(multcomp)
pvcmult = glht(pvc.anova, linfct = mcp(resin='Tukey'))
summary(pvcmult)

p.raw =  summary(pvc.anova)$coef[,4] # vector of individual p-values
p.raw = p.raw[order(p.raw)] # order the p-values
p.val = as.data.frame(p.raw) ; p.val
p.val$Bonferroni = p.adjust(p.val$p.raw, method="bonferroni")
p.val$Holm = p.adjust(p.val$p.raw, method="holm")
p.val$Hochberg = p.adjust(p.val$p.raw, method="hochberg")
p.val$BH = p.adjust(p.val$p.raw, method="BH")
p.val$BY = p.adjust(p.val$p.raw, method="BY") ; round(p.val,3)



