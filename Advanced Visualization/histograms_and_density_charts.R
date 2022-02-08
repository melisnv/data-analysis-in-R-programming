setwd("C:\\Users\\Melis Nur\\Desktop\\Basic-Statistics-in-R\datas/")
getwd()

data <- read.csv("datas/movie_rating_data.csv", stringsAsFactors = T)
colnames(data) <- c("Movie","Genre","CriticRating","AudienceRating","BudgetMillions","Year")
head(data)


# Histograms
s <- ggplot(data=data, aes(x= BudgetMillions))
s + geom_histogram(binwidth = 10)

# add color to border
s + geom_histogram(binwidth = 10, aes(colour='Black'))

# add color to inside of the histogram
s + geom_histogram(binwidth = 5,aes(fill=Genre))

# ------------------------------------------------------------
# Density Charts

s + geom_density()

s + geom_density(aes(fill=Genre))

# not very useful since examining it is difficult
s + geom_density(aes(fill=Genre),position = "stack")


# Layer tips

m <- ggplot(data=data, aes(x=AudienceRating))
m + geom_histogram(binwidth = 10, fill="White", colour="Blue")


# another way

m <- ggplot(data=data)
m + geom_histogram(binwidth = 10, aes(x=AudienceRating),fill="White", colour="DarkBlue")

# uniformly distributed
m <- ggplot(data = data)
m + geom_histogram(binwidth = 10, aes(x=CriticRating), fill="White",colour = "DarkRed")



# Statistical Transformations
# geom_smooth : Aids the eye in seeing patterns in the presence of overplotting.

n <- ggplot(data=data, aes(x=CriticRating, y=AudienceRating, colour=Genre))
n + geom_point() + geom_smooth() # by default it fills
n + geom_point() + geom_smooth(fill=NA)


# boxplots

v <- ggplot(data=data, aes(x=Genre, y = AudienceRating, colour=Genre))
v + geom_boxplot()

# size
v + geom_boxplot(size=1)

# random points
v + geom_boxplot() + geom_jitter()


# another way
v + geom_jitter() + geom_boxplot(alpha=0.5)

# -----------------------------------------------------
# The Boxplot Comment

# The highest rating could get with a thriller. It's got this the most the 
# narrowest box so it means the variance is very low. It's very risky
# to be in the horror movie business. On the other hand, drama genre is actually 
# also surprisingly very good which indicates that people kind of the like drama 
# these days as well as the romance and thriller.

# -------------------------------------------------------------------------------
# Using Facets
o <- ggplot(data=data, aes(x=BudgetMillions))
o + geom_histogram(binwidth = 10, aes(fill=Genre),colour="DarkBlue")

# reading this histogram is difficult since comparing the data is not possible

o + geom_histogram(binwidth = 10, aes(fill=Genre),colour="DarkBlue") +
  facet_grid(Genre~., scales="free")

# the one on the left represent rows, left represents columns and "." means nothing


w <- ggplot(data=data, aes(x=CriticRating, y=AudienceRating, colour=Genre))
w + geom_point(size=1.7) + facet_grid(Genre~.)

w + geom_point() + facet_grid(.~Year)

w + geom_point(aes(size=BudgetMillions)) + facet_grid(Genre~Year, scales = "free") + 
  geom_smooth()

# ----------------------------------------------------------------------------
# Coordinates

d <- ggplot(data=data, aes(x=CriticRating, y = AudienceRating,
                           size =BudgetMillions,
                           colour = Genre))
d + geom_point()

# specific parts to evaluate:
# won't work well all the time
d + geom_point() + xlim(50,100) + ylim(50,100)


l <- ggplot(data=data, aes(x=BudgetMillions))
l + geom_histogram(binwidth = 10, aes(fill=Genre), colour="Black") + ylim(0,50)

# instead use zooming :
l + geom_histogram(binwidth = 10, aes(fill=Genre), colour="Black") + 
  coord_cartesian(ylim = c(0,50))
  

w + geom_point(aes(size = BudgetMillions)) +
  geom_smooth() +
  facet_grid(Genre~Year) +
  coord_cartesian(ylim = c(0,100)) +
  scale_size_continuous(range=c(1,3))
  

  