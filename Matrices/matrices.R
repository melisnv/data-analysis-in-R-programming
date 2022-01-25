# Matrix creates a matrix from the given set of values.

# matrix()

my_data <- 1:20
my_data

A <- matrix(my_data, 4, 5)
A

new_indice <- A[2,3]
new_indice # 10

B <- matrix(my_data, 4, 5, byrow = TRUE)
B

indice_ten <- B[2,5]
indice_ten


# rbind()

r1 <- c('I','am','taking','R','programming')
r2 <- c('for','the','statistics','VU','course')
r3 <- c(1,2,3,4,5)
C <- rbind(r1, r2, r3)
C


# cbind()

c1 <- 1:5
c2 <- -1:-5
D <- cbind(c1 ,c2)
D

# which()

m <- matrix(1:9, 3, 3)
m

# usse which to get row and column containing the number 
which(m == 6, arr.ind = TRUE)
