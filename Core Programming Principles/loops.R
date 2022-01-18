# FOR LOOP

for (i in 1:5) {
  print("R Programming")
  
}

for (m in 10:20) {
  print(m)
}


# ------------------------

# WHILE LOOP
while(FALSE){
  
  print("Hello")
}

counter <- 1

while(counter < 12){
  print(counter)
  counter <- counter + 1
}


a <- 0
while (a < 6) {
  a <- a + 1
  if( a == 3){
    next
  }
  print(a)
}