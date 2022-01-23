
# An R function is created by using the keyword function. 
# The basic syntax of an R function definition is

function_name <- function(argument1,argument2){
  print(argument1)
  print(argument2)
}

function_name("Melis","Verir")


new_func <- function(a){
  for (i in 1:a) { # i = 1,2,3,4,5
    b <- i ^ 2
    print(i)
    print(b)
  }
}

new_func(5)



