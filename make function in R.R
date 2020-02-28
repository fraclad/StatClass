#create a function in R

myf = function(x){
  obj1 = x^2
  obj2 = x^3
  obj_combined = list('square' = obj1, 'cube' = obj2)
}

#create a vector, these are the same things made differently

vec_1 <- c(1,2,3,4,5)
vec_2 <- 1:5
vec_3 <- seq(1,4, by = 1)



print(myf(vec_1))
