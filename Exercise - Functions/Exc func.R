#create a function to calculate st dev
mysd = function(x){ 
  
  mean_x = mean(x)
  n = length(x)
  square_dif = c()
  
  for (i in 1:n){
    square_dif <- append(square_dif, (x[i] - mean_x)^2)
  }
  
  s = (sum(square_dif)/(n-1))^0.5
  return(s)
}

#vector to test
vec = 1:20

#test if self-made fnc is good, compare to built-in st dev func
mysd(vec) == sd(vec)

as.Date(Sys.Date(), format="%B %d %Y")
Sys.Date()

format(Sys.Date(), format="%B %d %Y")