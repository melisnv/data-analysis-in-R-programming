
#Test the Law of Large Numbers for N random normally distributed
# numbers with mean = 0, stdev =1.

# specify sample size
N <- 1000
count <- 0 # reset counter

# count how many of the numbers are between -1 and 1
for (i in rnorm(N)) {
  if(i >= -1 & i <= 1){
    count <- count + 1
  }
}

# calculate hit ratio
mean <- count / N*100

# check if mean > e(x)

mean
hist(mean)