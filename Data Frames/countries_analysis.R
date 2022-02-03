
# Importing data in R
# method : select the file manually

data <- read.csv(file.choose())
data

# method 2 : set working directory and read the file
# getwd returns an absolute file path representing the current working directory of the R process.
getwd()

# setwd(dir) is used to set the working directory to dir.
setwd("C:\\Users\\Melis Nur\\Desktop\\Basic-Statistics-in-R")
getwd()

# remove the existing data
rm(data)
data <- read.csv("Data Frames/demographic-data.csv")
data

# chr - Factors dilemma
stats <- read.csv("Data Frames/demographic-data.csv", stringsAsFactors = T)
# the factors will be created automatically

# ----------------------------------------------------------
# Exploring the data
# checking the how many col and rows there
nrow(data)
ncol(data)

# top rows
head(data)
head(data,n = 10)

# bottom rows
tail(data)
tail(data, n =8)

# a diagnostic function and an alternative to summary.
str(data)
str(stats)

# summary is a generic function used to produce result summaries of the results of various model fitting functions.
summary(data)
summary(stats)

# ----------------------------------------------------------
# using $ sign
data[3,3]
data[3,"Birth.rate"]
data[25,"Income.Group"]

data$Internet.users
data$Internet.users[3]
data[,"Internet.users"] # these are the same way

# levels provides access to the levels attribute of a variable.
levels(stats$Income.Group)





