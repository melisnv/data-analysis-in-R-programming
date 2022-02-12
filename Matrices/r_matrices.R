v <- 1:10 ; v

matrix(v)
matrix(v, nrow=5, ncol = 2)
matrix(v, nrow=2, ncol = 5, byrow = TRUE)

google <- c(450,580,600,650,800)
microsoft <- c(700,780,550,750,950)
stocks <- c(google,microsoft) ; stocks # combining

stock.matrix <- matrix(stocks,nrow = 2, byrow = TRUE) ; stock.matrix

#      [,1] [,2] [,3] [,4]
# [1,]  450  580  600  650
# [2,]  700  780  550  750

days <- c("Mon","Tue","Wed","Thur","Fri") ; days
companies <- c("MICROSOFT","GOOGLE") ; companies

colnames(stock.matrix) <- days
rownames(stock.matrix) <- companies
stock.matrix

#           Mon Tue Wed Thur Fri
# MICROSOFT 450 580 600  650 800
# GOOGLE    700 780 550  750 950

# -----------------------------------------

mat <- matrix(1:25, byrow = TRUE, nrow = 5) ; mat
mat* 2 ; mat
mat / 2

# filtering the matrice
mat > 15
#       [,1]  [,2]  [,3]  [,4]  [,5]
# [1,] FALSE FALSE FALSE FALSE FALSE
# [2,] FALSE FALSE FALSE FALSE FALSE
# [3,] FALSE FALSE FALSE FALSE FALSE
# [4,]  TRUE  TRUE  TRUE  TRUE  TRUE
# [5,]  TRUE  TRUE  TRUE  TRUE  TRUE

mat[mat > 15]

# matrix multiplication
mat %*% mat

# -------------------------------------------

stock.matrix
# total sum
colSums(stock.matrix)
# Mon  Tue  Wed Thur  Fri 
# 1150 1360 1150 1400 1750 

rowMeans(stock.matrix)

FACEBOOK <- c(300,450,350,600,640) ; FACEBOOK
tech.stocks <- rbind(stock.matrix, FACEBOOK) ; tech.stocks # adds a new vector as a row in matrix


avg <- rowMeans(tech.stocks) ; avg # adding average to the matrix
tech.stocks <- cbind(tech.stocks, avg) ; tech.stocks

# ---------------------------------------------------------
# Indexing and Slicing in Matrices

m <- matrix(1:50, byrow = TRUE, nrow = 5) ; m
m[1,]
m[,1]

mat[1:3,1:2]

# ---------------------------------------------------------
# Factor and Categorical Matrices

animals <- c("d","c","d","c","c","d","d") ; animals
id <- c (1,2,3,4,5,6,7) ; id

factor(animals)
# d c d c c d d
# Levels: c d

factor.animal <- factor(animals) ; factor.animal

# Nominal (No-order) - Ordinal Factors

temp <- c("hot","cold","medium","hot","cold","hot","medium") ; temp
factor.temp <- factor(temp, levels = c("cold","medium","hot"), ordered = TRUE) ; factor.temp
summary(factor.temp)
# cold medium    hot 
#  2      2      3 

summary(temp)
# Length     Class      Mode 
#   7       character  character 



matrix(runif(20),byrow = TRUE, nrow = 4)
