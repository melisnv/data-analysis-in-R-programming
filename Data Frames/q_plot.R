# reading the data
setwd("C:\\Users\\Melis Nur\\Desktop\\Basic-Statistics-in-R\Data Frames/")
getwd()

data <- read.csv("Data Frames/demographic-data.csv", stringsAsFactors = T)
data

data[data$Country.Name == "Turkey",]

# working with qplot

library(ggplot2)

qplot(data = data, x=Internet.users)
# to assign a value to design the plot, wrap it with I()
qplot(data = data, x= Income.Group, y = Birth.rate, size=I(3))

qplot(data = data, x = Income.Group, y = Birth.rate,
      colour = I("pink"), geom = "boxplot")

qplot(data = data, x = Income.Group, y = Birth.rate, geom = "boxplot")


# visualizing
# categorize to the scatterplot by the income groups

qplot(data = data, x = Internet.users, y = Birth.rate)

qplot(data = data, x = Internet.users, y = Birth.rate, size= I(2),
      colour = Income.Group)







