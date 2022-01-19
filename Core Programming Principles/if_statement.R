# rnorm function generates a set of numbers which are randomly distributed
# according to the normal distribution

rm(answer) # remove the variable if exists
x <- rnorm(1)

if(x > 1){
  answer <- "Greater than 1"
} else {
  
  # Nested if-else statement
  if(x >= -1){
    answer <- "Between -1 and 1"
    
  } else {
    answer <- "Less than -1"
  }
  
}

# chaining if-else instead of 
# nesting is a better programming approach

rm(result) # remove the variable if exists
y <- rnorm(1)

if(y > 1){
  result <- "Greater than 1"
} else if(y >= -1){
  result <- "Between -1 and 1"
} else {
  result <- "Less than -1"
}