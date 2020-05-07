data = rnorm(30, mean = 10, sd = 15)
data2 = rnorm(30, 8, 15)

t.test(y = data, x = data2, NULL = 0, var.equal = T) # no difference in the means og the two samples
# Welch test if variance equivalence is not asserted
