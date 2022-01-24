# Installing packages

install.packages("ggplot2")

library(ggplot2)

?diamonds  # a dataset

qplot(data=diamonds, carat, price, colour= clarity, facets = .~clarity)
