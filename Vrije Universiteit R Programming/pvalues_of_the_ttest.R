
p_value = function(n,m,mu,nu,sd,B=1000){
  
  p=numeric(B) # p will be an array of realized p-values
  
  for (b in 1:B) {
    x=rnorm(n,mu,sd)
    y=rnorm(m,nu,sd)
    p[b]=t.test(x,y,var.equal=TRUE)[[3]]
  }
  return(p)
  
}

n=m=30; mu=nu=180; sd=10
p=p_value(n,m,mu,nu,sd) ; p

# visualization
par(mfrow=c(1,2))
hist(p, freq = FALSE, main = "Histogram of P-value with sd = 10")

n=m=30; mu=nu=180; sd=1
k=p_value(n,m,mu,nu,sd) ; p

# visualization
par(mfrow=c(1,2))
hist(k, freq = FALSE, main = "Histogram of P-value with sd = 10")
