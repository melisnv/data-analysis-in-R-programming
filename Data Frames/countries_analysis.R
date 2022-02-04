
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

# ----------------------------------------------------------
# basic operations


stats[1:10,] # 10 rows,all the columns
stats[1,]
is.data.frame(stats[1,])

stats[,1]
is.data.frame(stats[,1]) #FALSE

stats[,1,drop=F]
is.data.frame(stats[,1,drop=F])

# multiple columns
head(stats)

# adding a column
stats$People_Rate <- stats$Birth.rate * stats$Internet.users
head(stats)

# removing a column
stats$People_Rate <- NULL #removed
head(stats,n=10)

#----------------------------------------------------------
# filtering dataframes

filter <- stats$Internet.users < 20 # returned vector
stats[filter,]


filter2 <- stats[stats$Birth.rate > 40,]
filter2

filter3 <- stats[stats$Birth.rate > 40 & stats$Internet.users < 10,]
filter3

stats[stats$Income.Group == "High income",]

# finding malta
malta_country <- stats[stats$Country.Name == "Malta",]
malta_country

# countries whose second chr is 'a'
specific_countries <- stats[substr(stats$Country.Name,2,2) == 'a',]
specific_countries


t_countries <- stats[substr(stats$Country.Code,1,1) == 'T',]
t_countries


