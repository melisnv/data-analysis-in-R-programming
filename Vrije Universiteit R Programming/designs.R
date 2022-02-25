# The rows of the data frame ashinal correspond to 16 subjects and give measures of pain (for chronic headache) when treated with a drug (a) (that 
# inhibits nitric oxide synthase) or a placebo (p). The bigger the outcome pain, the more the measured headache. One of the three columns sequence,
# treatment and period is redundant, but useful for the analysis.

ashinal <- read.table("datas/ashinal.txt", header =TRUE)
ashinal

summary(ashinal)

ashinal$id = as.factor(ashinal$id) ; ashinal$period = as.factor(ashinal$period)
ashinal.model = lm(pain~treatment+period+id, data = ashinal)
anova(ashinal.model)
summary(ashinal.model)

# In general, changing the order of factors in the anova formula gives different p-values, as anova performs “sequential tests”. To use it correctly,
# put the factor of interest last in the formula.

anova(lm(pain~id+period+treatment, data = ashinal))
summary(lm(pain~id+period+treatment, data = ashinal))

# Mixed Effects
# lme4, provides functions for fitting and analyzing mixed models. The function lmer gives the correct implementation of the crossover design,
# with the individuals as “random effects”.
library(lme4)

ashinalModel <- lmer(pain~treatment+sequence+period+(1|id), data = ashinal ,REML = FALSE)
summary(ashinalModel)

ashinalModel2 <- lmer(pain~sequence+period+(1|id),data=ashinal,REML=FALSE)
anova(ashinalModel2,ashinalModel)

# Split-Plot Design


wheat <- read.table("datas/wheat.txt",header = TRUE)
wheat

# fixed effects
wheat$spray = as.factor(wheat$spray) ; wheat$variety = as.factor(wheat$variety) 
wheat.model <- lm(yield~spray*variety+farm+farm:spray, data = wheat)
anova(wheat.model)
summary(wheat.model)

# mixed effects
wheatlmer=lmer(yield~spray*variety+(1|farm)+(1|farm:spray),data=wheat,REML=FALSE)
summary(wheatlmer)

# Recall that we cannot directly run anova(wheatlmer) to test for any factor of interest. We need to create a model without that factor and test
# that model inside the full one. For example, to test the effect of the factor variety we fit the mixed effects model
# again, now without this factor in wheatlmer1 and test by anova its fit within the full model wheatlmer.

wheatlmer2 = lmer(yield~spray+(1|farm)+(1|farm:spray),data=wheat,REML=FALSE)
anova(wheatlmer2, wheatlmer)
