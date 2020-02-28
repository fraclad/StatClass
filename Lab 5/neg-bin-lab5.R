#P(Y=10), Y~NegBin(p=0.4,r=3).   (Book theory)
#  This means that in R we need n=3 (size), x=y-r=10-3=7
dnbinom(7,3,0.4)  # Nu Failures, Nu of successes, prob success

mynbin=function(y,r,p){
choose(y-1,r-1)*p^r*(1-p)^(y-r)
}

mynbin(10,3,0.4)

dbinom(2,10,0.1)

help("dmultinom")
dmultinom(prob = c(15,27,31,19,11)/103),
