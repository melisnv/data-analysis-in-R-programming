# numeric
vector.numeric = c(1,2,3,4,5,6)
class(vector.numeric)

#  character
vector.char = c("T","R")
class(vector.char)

# logical
vector.bool = c(TRUE,FALSE)
class(vector.bool)

# mixed data types in vector is not allowed
vec <- c(TRUE, 20,40) ; vec
class(vec)


temps <- c(15,13,9,8,12,5,2) ; temps
names(temps) <- c("Mon","Tues","Wed","Thur","Fri","Sat","Sun") ; temps
# each weather degree now assigned to a day, output:
#  Mon Tues  Wed Thur  Fri  Sat  Sun 
#   15   13    9    8   12    5    2 

# second option to construct another vector and assign it
days <- c("Mon","Tues","Wed","Thur","Fri","Sat","Sun") ; days
names(temps) <- days ; temps


# Operations

v1 <- c(10,15,20)
v2 <- c(2,4,6)

# element by element
sum = v1 + v2 ; sum

# built-in functions
sum(v1,v2)
prod(v1) # returns the product of all the values present in its arguments.

# ---------------------------------------
# Comparison Operators

2 == 5 # equality

res <- v1 < 5 ; res

# ---------------------------------------
# Vector Indexing and Slicing

vec1 <- c(200,300,400)
vec2 <- c("a","b","c")
vec1[2]

# slicing
vec1[c(1,3)]

vec2[c(1,3)]

nums <- c(1,2,3,4,5,6,7,8,9,10) ; nums

nums[2:6]
nums[1:10]

v <- c(1,2,3,4) ; v
alp <- c("a","b","c","d") ; alp

names(v) <- alp ; v

v[c("c","d","a")] ; v

# filtering with comparison operator
v[v>2]








