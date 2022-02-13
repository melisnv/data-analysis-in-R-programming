# Histogram

x = rnorm(100) ; x
par(mfrow=c(1,2)) # two plots next to each other
hist(x) # frequencies on y-axis
hist(x, probability = TRUE) # probabilities on y-axis

# The histogram of a sample varies around p. 
# The smaller the sample, the bigger this variation.

# -------------------------------------------------------------------
# Crime Data in USA 

data = read.table("datas/expensescrime(1).txt", header = TRUE)
head(data)

summary(data)
str(data)

# The correlation between all pairs of variables, excluding the first column
round(cor(data[,-1]),3)

# A matrix of scatterplot
pairs(data[,-1])

pairs(data[c(2,4,6,7)]) # selects columns 2,4,6,7 of data

# histograms
par(mfrow=c(1,2))
hist(data$expend,main = "Expend", xlab = "Expend")
hist(data$crime, main = "Crime Rate", xlab = "Crime")

# boxplots
boxplot(data$expend, main="Expend", col = "blue",border = "black")
boxplot(data$crime, main="Crime Rate", col = "orange",border = "red")

qqnorm(data$expend, main = "Expend")
qqnorm(data$crime,main="Crime Rate")
