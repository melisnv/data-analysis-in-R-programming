# Analyze the data in a three-way experiment without interactions with acidity as response and starter, batch and position as factors. 
# By using summary command, can you tell whether there is a significant difference between the effects of starter 1 and starter 2 on acidity?

creamdata <- read.table("datas/cream.txt", header = TRUE)
creamdata

creamdata$starter = as.factor(creamdata$starter) ; creamdata$position = as.factor(creamdata$position) ; 
creamdata$batch = as.factor(creamdata$batch) ; 

# mixed effect
creamModel <- lmer(acidity~starter+position+(1|batch), data = creamdata, REML=FALSE)
summary(creamModel)

creamModel2 <- lmer(acidity~position+(1|batch), data = creamdata, REML=FALSE)
anova(creamModel2,creamModel) # testing models and comparing them

