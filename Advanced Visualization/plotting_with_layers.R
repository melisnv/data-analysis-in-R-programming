setwd("C:\\Users\\Melis Nur\\Desktop\\Basic-Statistics-in-R\datas/")
getwd()

data <- read.csv("datas/movie_rating_data.csv", stringsAsFactors = T)
head(data)

# --------- Aesthetics
# Object
p <- ggplot(data=data,aes(x=CriticRating, y=AudienceRating, colour=Genre, size=BudgetMillions)) # list of 9

# point
p + geom_point()

# lines
p + geom_line()

# multiple layers
p + geom_line() + geom_point()

# ---------------------------------------------------
# Overriding aesthetics


q <- ggplot(data=data, aes(x = CriticRating, y = AudienceRating, size = BudgetMillions,
                           colour=Genre))

q + geom_point()

# overriding
q + geom_point(aes(size = CriticRating))

# overriding the color
q + geom_point(aes(colour=BudgetMillions))

# Not modifying the object q, it's not changing and remains the same

q + geom_point(aes(x=BudgetMillions)) + xlab("Budget Millions")

# setting aesthetics and overriding with layers
q + geom_line(size=0.5) + geom_point()


# Mapping vs Setting

r <- ggplot(data = data, aes(x= CriticRating, y=AudienceRating))
r + geom_point()

# mapping
r + geom_point(aes(colour=Genre))

# setting
r + geom_point(colour="DarkGreen")

# the size:
# mapping
r + geom_point(aes(size=BudgetMillions))

# setting
r + geom_point(size=5, colour="DarkBlue")

# ERROR : 
r + geom_point(aes(size=10)) # do not use this like this, use it if you'll do mapping
