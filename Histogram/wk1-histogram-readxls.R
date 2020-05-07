dird='C:/Users/ra/Documents/StatClass/Histogram/'

library(readxl)

myread=function(x) 
{
  library(readxl)
  read_xls(paste0(dird,x))
}

ants = myread("GOBIANTS.xls")
ants

#ants=read.csv(file.choose())

#remove 8th row
ants2=ants[-8,] #this is the syntax to remove the 8th row
ants2
windows()#quartz()
with(ants2, coplot(Diversity~Rain|Region))

library(ggplot2)
g = ggplot(ants2, aes(x=Region, y=Rain, fill=Region))
g = g + geom_violin()
g
#see whats in the data frame
ants2

#Look at diversity index
D=ants2$Diversity
D

#Say we want 3 bins
#what is each width?
inc=((max(D)+ 0.1)-0)/3

#Make some class breaks
cl=seq(0,max(D)+0.1,by=inc)
cl


# sort them
sort(D)


#Cut the diversity three places
Dc=cut(D,breaks=cl,ord=TRUE)
Dc
D

# What is its class?
class(Dc)

#Make a table
tab=table(Dc)
tab

#Make a barplot
windows()
barplot(tab)


# embellish
windows()

barplot(tab,space=0, col=rainbow(3), main="Histogram of Ant diversity", 
        ylab="Frequency",xlab="Diversity")

#The easy way
hist(D,nc=3, col=rainbow(3))

# Relative frequency histogram
barplot(tab/sum(tab),space=0, col=rainbow(3), main="Histogram of Ant diversity", 
        ylab="Rel. Frequency",xlab="Diversity")

# Density hist (total area =1)
barplot(tab/(inc*sum(tab)),space=0, col=rainbow(3), main="Histogram of Ant diversity", 
        ylab="Density",xlab="Diversity")


#Check density
tab/(inc*sum(tab))*inc


stem(D)

head(ants2)

with(ants2,boxplot(Diversity~Region))

# standard deviation of the sample
sd(D)

#Range
range(D)

#epagas.df=read.csv(file.choose())
epagas.df = myread("EPAGAS.xls")
head(epagas.df)
with(epagas.df, stripchart(MPG, method="stack",pch=21,offset=1/3))
with(epagas.df, {
  stem(MPG,scale=2)
  sort(MPG)
}
)
# Three types of histogram a) Frequency b) rel., Freq and c) Density
classrange=45.0-29.9
inc=classrange/10
bins=seq(29.9,45.0,by=inc)
bins
with(epagas.df,cut(MPG,bins))
cl= with(epagas.df,cut(MPG,bins))
cl.tab=table(cl)
cl.tab
barplot(cl.tab)
epagas.df$MPG->mpg
length(mpg)
n=length(mpg)
barplot(cl.tab,las=2,main="Frequency Histogram",ylab="Frequency",space=0)
barplot(cl.tab/n,las=2,main="Relative Frequency Histogram",ylab="Relative frequency",space=0)
barplot(cl.tab/(n*inc),las=2,main="Density Histogram",ylab="Density",space=0) 
sum(cl.tab/n)
#Area of density
h=cl.tab/(n*inc)
A=sum(h*inc)
A

### Chapter 2 Example 2.2 IRONORE
#ironore.df=read.csv(file.choose())
ironore.df = myread("IRONORE.xls")
head(ironore.df)
h<-with(ironore.df, hist(IRONORE,labels=TRUE,col="Blue",las=1,probability=TRUE))
h
sum(h$density*0.5)
iron=ironore.df$IRONORE

sd(iron)  #sd
sd(iron)^2  #var
sum((iron-mean(iron))^2/(length(iron)-1))
summary(iron)
hist(iron)


x=iron
## Section 2.5 IRONORE  CHEBYCHEV
s=sd(x) 
mn=mean(x)
mn-s
k=1
sum(x>=mn-k*s & x<=mn+k*s)/length(x)*100
paste("(",mn-k*s,",",mn+k*s,")") 
#chebyshev 1-1/k^2 = 0
k=2
sum(x>=mn-k*s & x<=mn+k*s)/length(x)*100
paste("(",mn-k*s,",",mn+k*s,")") 
#chebyshev 1-1/k^2 = 1-1/4=3/4
k=3
sum(x>=mn-k*s & x<=mn+k*s)/length(x)*100  
paste("(",mn-k*s,",",mn+k*s,")")                                     
#chebyshev 1-1/k^2 = 1-1/9=8/9 =0.89


## Calculus
Rn=function(b=1,n=10){
  dx=1/n
  x=seq(0,b,by=dx)
  area=sum(dx*x[-1]^2)
  h=x[-1]^2
  xx=x[-1]
  names(xx)=x[-1]
  barplot(xx,las=2,space=0)
  area
}
Rn(n=10)
