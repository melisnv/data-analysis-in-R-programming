# Analyze the data in a three-way experiment without interactions with acidity as response and starter, batch and position as factors. 
# By using summary command, can you tell whether there is a significant difference between the effects of starter 1 and starter 2 on acidity?
library(lme4)

creamdata <- read.table("datas/cream.txt", header = TRUE)
creamdata

# 3-way
creamdata$starter=factor(creamdata$starter) ; creamdata$position=factor(creamdata$position)
creamModel=lm(acidity~starter+batch+position,data=creamdata)
anova(creamModel)
summary(creamModel)

# Recall that the main interest is in the effect of starter on the acidity; factors positions and batches represent the block variables. 
# Remove insignificant block variable(s) if there are such, and perform an ANOVA for the resulting “fixed effects” model. 
# Which starter(s) lead to significantly different acidity? 
creamModel2=lm(acidity~starter*batch+position,data=creamdata)
anova(creamModel2)
summary(creamModel2)


# For the model from b), can we also apply the Friedman test to test whether there is an effect of starter on acidity? Motivate your answer. 


# Repeat b) by performing a mixed effects analysis, modeling the block variable(s) (if there are any) as a random effect by using the function lmer. 
# Compare your results to the results found by using the fixed effects model in b). 

creamlmer=lmer(acidity~starter+position+(1|batch), data=creamdata, REML=FALSE)
summary(creamlmer)

              
            


