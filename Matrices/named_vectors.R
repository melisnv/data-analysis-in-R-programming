#Named Vectors

Universities <- 1:5
Universities

# Giving names
names(Universities) <- c("Leiden University","University of Amsterdam","Vrije Universiteit Amsterdam","Radboud University","University of Groningen")
Universities

# To reach the Vrije Universiteit Amsterdam
Universities[3]
names(Universities)


# Clear names
names(Universities) <- NULL
Universities
names(Universities)



# Naming Matrix Dimensions
rep(c("a","B","Zz"),times=3)
rep(c("a","B","Zz"),each = 3)

temp.vec <- rep(c("a","B","Zz"), each = 3)
temp.vec

new_matrix <- matrix(temp.vec, 3, 3)
new_matrix


# row.names() and colnames()
row.names(new_matrix) # NULL

row.names(new_matrix) <- c("European",'African',"Asian")
new_matrix

colnames(new_matrix) <- c("Denmark","Namibya","India")
new_matrix

new_matrix["European","Denmark"] # a
new_matrix["European","Denmark"] <- "Nordic"
new_matrix  

row.names(new_matrix)  
row.names(new_matrix) <- NULL  
new_matrix  
  
  
