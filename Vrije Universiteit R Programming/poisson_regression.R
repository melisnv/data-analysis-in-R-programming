galapagos <- read.table("datas/gala.txt", header=TRUE)
galapagos

pairs(galapagos)

galapagosglm <- glm(Species~Area+Elevation+Nearest+Scruz+Adjacent, family = poisson, data = galapagos)
summary(galapagosglm)
