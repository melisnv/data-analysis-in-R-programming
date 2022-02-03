# Subsetting

x <- c("a","b","c","d","e")
x

x[c(1,5)]
x[1]

# Subsetting the Matrices
Games

# top payed players in the NBA analyzing the number of plays they played for last 5 years
Games[1:3,6:10]


Games[c(3,5),]
Games[,c("2012","2014")]

# This returns vector even though it's matrice
Games[5,6]
is.vector(Games[5,6]) # TRUE

# not letting R to drop the dimension
Games[1,,drop=F] # getting back the matrice

#  retrieve values from greater than 2009
Games[, as.numeric(colnames(Games)) > 2009]

#  retrieve values from 2009 and less than or equal to 2011
Games[, as.numeric(colnames(Games)) > 2009 & as.numeric(colnames(Games)) <= 2012]


# Visualizing the Subsets
# "b" for both points and lines
Data <- MinutesPlayed[1:3,]
matplot(t(Data), type = 'b', pch = 15:18, col=c(1:4,6))
legend("bottomleft",inset = 0.01, cex = 0.3, horiz = F,
       legend = Players[1:3], pch = 15:18, col = c(1:4,6))



# Only with Kobe

Data_Kobe <- MinutesPlayed[1,,drop=F]
Data_Kobe
matplot(t(Data_Kobe), type = 'b', pch= 15:18, col=c(1:4,6))
legend("bottomleft", inset = 0.01, legend = Players[1], 
       col=c(1:4,6), pch = 15:18, horiz = F, cex = 0.3)



# default parameter row=1:10
myfunc <- function(data,row = 1:10){
  
  Data <- data[row,, drop=F]
  matplot(t(Data), type = 'b', pch = 15:18, col=c(1:4,6))
  legend("bottomleft",inset = 0.01, cex = 0.3, horiz = F,
         legend = Players[row], pch = 15:18, col = c(1:4,6))
  
}


myfunc(Salary)
myfunc(MinutesPlayed/Games,3)



