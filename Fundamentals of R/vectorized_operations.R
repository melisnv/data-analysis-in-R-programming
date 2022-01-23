m <- rnorm(10)
m

# R specific programming loop
for(i in m){
  print(i)
}

print(m[1])
print(m[2])

# conventional programming loop
for(j in 1:5){
  print(m[j])
}

N <- 100
a <- rnorm(N)
b <- rnorm(N)

# Vectorized approach
c <- a * b
c

# De-vectorized approach:
# go over all the elements and multiply them one by one.

d <- rep(NA,N)  # create an empty vector
d

for(i in 1:N){
  d[i] <- a[i] * b[i]
}

is_equal <- d == c
is_equal



