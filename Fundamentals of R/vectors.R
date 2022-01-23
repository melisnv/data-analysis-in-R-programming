
# Vectors are an ordered set. Starts with index 1.
# Even a single number stored as a vector in R with length 1.


first_vector <- c(7, 65, 32, 48, 120, 800)
first_vector

is.numeric(first_vector) # allows to check if an object is numeric

# By default R store numbers as double not integer
is.integer(first_vector) 

is.double(first_vector) # This returns true

second_vector <- c(4L, 9L, 12L, 32L, 56L)
second_vector

is.integer(second_vector) # True because it is specified as L


# Character vector
third_vector <- c("Melis", "Nur", "Ayse", "Atila", "Amsterdam", "Istanbul")
third_vector

is.character(third_vector)
is.numeric(third_vector)

# It it not possible to put different data types in the same vector.

# Sequence
seq(1,10) # 1:10
seq(1,20,3) # last argument is the step

z <- seq(1,30,2)
z


# Replicate
rep(3, 5)
m <- rep(2,24)
m

character_rep <- rep("melis",5)
character_rep


# Using replicate on vectors/arrays/lists
new_vector <- c(2,4,6,8,10,12,14,16,18,20)
rep_vec <- rep(new_vector,5)
rep_vec


x <- c(1,2,3,4,5,6,7,2,3,4,5,5)
y <- seq(201, 650,10)
t <- rep('Hello',3)


# accessing the elements of a vector
w <- c("a","b","c","d","e","f","g")
w
w[1] # returns the first element
# accessing all the elements of a vector except the first one
w[-1]

vector_without_c <- w[-3]
vector_without_c


# accessing the multiple elements of a vector
w[1:3]

w[c(1,3,5)]

w[-3:-5]

# Vector operations
a <- c(1,2,3,4,5,6,7,8,9,10)
b <- c(9,8,7,6,5,4,3,2,1,0)
c <- a + b
c



