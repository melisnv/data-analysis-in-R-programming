setwd("C:\\Users\\Melis Nur\\Desktop\\Basic-Statistics-in-R\datas/")
getwd()

data <- read.csv("datas/movie_rating_data.csv", stringsAsFactors = T)
head(data)

colnames(data) <- c("Movie","Genre","CriticRating","AudienceRating","BudgetMillions","Year")
head(data)
tail(data)

str(data)
summary(data)

# Year column should be a factor so that it can be comparable
# encode a vector as a factor (the terms ‘category’ and ‘enumerated type’ are also used for factors).
factor(data$Year)
data$Year <- factor(data$Year) # assigning a column to a vector
summary(data)

# --------- Aesthetics

library(ggplot2)

ggplot(data=data,aes(x=CriticRating, y=AudienceRating))

# add geometry
ggplot(data=data,aes(x=CriticRating, y=AudienceRating)) + geom_point()

# add colour
ggplot(data=data,aes(x=CriticRating, y=AudienceRating, colour=Genre)) + geom_point()

# add size
ggplot(data=data,aes(x=CriticRating, y=AudienceRating, colour=Genre, size=Genre)) + geom_point()


# add size better way
ggplot(data=data,aes(x=CriticRating, y=AudienceRating, colour=Genre, size=BudgetMillions)) + geom_point()















