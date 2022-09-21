myName <- "Yingnan Lyu"

#1
#1(a)
v1 <- c(1:20)
#1(b)
v2 <- c(20:1)
#1(c)
v3 <- seq(from=1, to=19, by=2)
#1(d)
v4 <- rep(c(3,7,11), times=10)
#1(e)
v <- rep(c(3,7,11), times=10)
v5 <- c(v,3)


#2
a <- seq(3,6,by=0.1)
x1=exp(a)*sin(a)

#3
b <- 10:100
sum1=sum(b^3+4*b^2)

#4
#4(a)
str1=paste("label", 1:30)
#4(b)
str2=paste("function", 1:30,sep="")

#5
vs <- c(1,'function',NA, seq(1,5,2), 0.125)
vs <- paste(vs, collapse = ",")


#matrix
#6
A <- matrix(1:9, 3,3)
m1_ans <- A%*%A%*%A

#7
B <- matrix(c(12,-12,12), b=T, nc=3, nr=17)
m2_ans <- t(B) %*% B

#8
yVec <- c(7,-1,-3,5,17)
AMat <- matrix(0,nr=5, nc=5)
AMat <- abs(col(AMat)-row(AMat))+1
m3_ans <- solve(AMat)%*%yVec
m3_ans

#9
#9(a)
xVec <- seq(0.0, 1.0, by= 0.1); xVec

function1 <- function(xVec)
{
  xVec^(1:length(xVec))
}

func1_ans = function1(xVec)

#9(b)

function2 <- function(xVec)
{
  (xVec^(1:length(xVec))/(1:length(xVec)))
}

func2_ans = function2(xVec)

#9(c)

function3 <- function(x,n)
{
  1+sum((x^(1:n))/(1:n))
}
func3_ans <- function3(seq(0,1,0.1),length(seq(0,1,0.1)))
func3_ans
#10
cel_to_far <- function(tp)
{
  return((tp*1.8)+32)
}
cel_to_far(30)

far_to_cel <- function(tp)
{
  return((tp-32)*(5/9))
}
far_to_cel(50)

#11
n <- 1:2000
odd_ans <- seq(from=1,to=2000,by=2) 
odd_ans
#12
function12<- function (n)
{
 f12 <- function(r){sum(((1:r)^0.5)/(11+3.5*r^1.2))}
  sum(sapply(1:n, FUN=f12))
}
sum_ans <- function(n)
  sum_ans

#13
modNumber <- function(x, y)
{
  while(x %% y != 0){
    x = x + 1
  }
  xy <- x
}
#14
numberOfWheels <- function(x){
  n <- switch(x, "unicycle"=1, "bike"=2,"car"=4, "truck"=4, "tricycle"=3, "motorcycle"=2)
 return(n)
}
  numberOfWheels
  
#15

myFactorial <- function(n){
  if(n <= 1){
    return(1)
  }else{
    return(n*myFactorial(n-1))
  }
}

#16
myCustomFactorial <- function(x,y){
  prod(y:x)
}

#17
customRiverMean <- function(maxl){
  df <- rivers
  y <- (rivers[rivers < maxl])
  opt <- mean(y)
  return(opt)
}

#18









