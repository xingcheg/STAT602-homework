library(ggplot2)


## 1.6
runif_p <- function(n,p){
  X <- matrix(runif(n*p,0,1), ncol=p, nrow=n)
  return(X)
}

dist_n <- function(X){
  n <- length(X[,1])
  dist <- NULL
  for ( i in 1:(n-1) ){
    for ( j in (i+1):n ){
      x1 <- X[i,]
      x2 <- X[j,]
      d <- sqrt(sum((x1-x2)^2))
      dist <- c(dist, d)
    }
  }
  return(dist)
}

hist( dist_n( runif_p(100,2)  ),  xlim=c(0,1.5) )
hist( dist_n( runif_p(100,10)  ), xlim=c(0,2.2)  )
hist( dist_n( runif_p(100,100)  ), xlim=c(0,5)  )
hist( dist_n( runif_p(100,1000)  ), xlim=c(0,14) )





## 1.7
vol_ball <- function(p,r){
  A <- pi^(p/2)
  f <- function(x,a){
    return( x^(a-1)*exp(-x) )
    }
  B <- integrate(f=f, lower=0, upper=Inf, a=(1 + p/2))
  B <- B$value
  Vp <- (A/B)*(r^p)
  return(Vp)
}

fillup_lb_rate <- function(p,r){
  R <- (p/2)*log10(p/(2*pi*exp(1)*r^2)) + (1/2)*log10(p*pi)
  return(R)
}


1/vol_ball(20,1)
1/vol_ball(50,1)
# 1/vol_ball(200,1)

1/vol_ball(20,0.1)
1/vol_ball(50,0.1)
# 1/vol_ball(200,0.1)

1/vol_ball(20,0.01)
1/vol_ball(50,0.01)
# 1/vol_ball(200,0.01)

fillup_lb_rate(20,1)
fillup_lb_rate(50,1)
fillup_lb_rate(200,1)

fillup_lb_rate(20,0.1)
fillup_lb_rate(50,0.1)
fillup_lb_rate(200,0.1)

fillup_lb_rate(20,0.01)
fillup_lb_rate(50,0.01)
fillup_lb_rate(200,0.01)






## 3.4
library(readxl)
D3_4 <- read_excel(path="/Users/apple/Desktop/ISU 2019 spring/STAT 602/hw/hw1/Problem3.4Data.xlsx")
D3_4 <- as.data.frame(D3_4)


cv_10fd <- function(data,formula){
  cv_score <- NULL
  for (i in 1:10){
    test_ind <- (1:10)+(i-1)*10
    Train <- data[-test_ind,] 
    Test <- data[test_ind,] 
    fit <- lm(formula=formula,data=Train)
    py <- predict(fit,newdata=data.frame(x=Test$x))
    MSE <- mean(  (py - Test$y)^2  )
    cv_score <- c(cv_score,MSE)
  }
  return(mean(cv_score))
}

formula1.0 <- "y~ 1"
cv_10fd(D3_4, formula1.0)

formula1.1 <- "y~ x"
cv_10fd(D3_4, formula1.1)

formula1.2 <- "y~ x + I(x^2)"
cv_10fd(D3_4, formula1.2)

formula1.3 <- "y~ x + I(x^2) + I(x^3)"
cv_10fd(D3_4, formula1.3)

formula1.4 <- "y~ x + I(x^2) + I(x^3) + I(x^4)"
cv_10fd(D3_4, formula1.4)

formula1.5 <- "y~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5)"
cv_10fd(D3_4, formula1.5)

formula2 <- "y~ sin(x) + cos(x)"
cv_10fd(D3_4, formula2)

formula3 <- "y~ sin(x) + cos(x) + sin(2*x) + cos(2*x)"
cv_10fd(D3_4, formula3)

formula4 <- "y~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + sin(x) + cos(x) + sin(2*x) + cos(2*x)"
cv_10fd(D3_4, formula4)





## 3.11
#a
P1x <- function(x){
  l1 <- dnorm(x,1,1/2)
  l2 <- dnorm(x,0,1)
  out <- l1/(l1+l2)
  return(out)
}


x <- seq(-2,4,0.001)
plot(x,P1x(x),type="l")

#b
x <- seq(-2,4,0.001)
plot(x,P1x(x),type="l")
abline(h=0.5,col="red")


range( x[which( P1x(x)>=0.5 )] )
# 0.382 2.285

## ERR
(1/2)*( pnorm(2.285,0,1)-pnorm(0.382,0,1) ) + 
  (1/2)*( 1-pnorm(2.285,1,1/2) + pnorm(0.382,1,1/2) )
#  0.2266942

#c
Rlc <- function(a){
  out <- (1/2)*pnorm(a,0,1) + (1/2)*(1-pnorm(a,1,0.5))
  return(out)
}

Ruc <- function(a){
  out <- (1/2)*(1-pnorm(a,0,1)) + (1/2)*pnorm(a,1,0.5)
  return(out)
}

x <- seq(-2,8,0.001)
plot(x,Rlc(x),type="l",col="red", ylim=c(0.1,0.8))
lines(x,Ruc(x),type="l",col="blue")

x[which.min(Ruc(x))]

## ERR
(1/2)*( 1-pnorm(0.382,0,1) ) + 
  (1/2)*(  pnorm(0.382,1,1/2) )
#  0.22973


##d
M <- 10000
N1 <- 100
N2 <-50



simu_xy <- function(N){
  y <- rbinom(N,1,1/2)
  x0 <- rnorm(N,0,1)
  x1 <- rnorm(N,1,1/2)
  x <- x0*(1-y) + x1*y
  out <- data.frame(x=x,y=y)
  return(out)
}


Rlc1 <- function(a,x,y){
  I1 <- (y==0)&(x<a)
  I2 <- (y==1)&(x>a)
  out <- sum(I1)+sum(I2)
  return(out)
}

Ruc1 <- function(a,x,y){
  I1 <- (y==0)&(x>a)
  I2 <- (y==1)&(x<a)
  out <- sum(I1)+sum(I2)
  return(out)
}


classifier_3_11d <- function(M,N){
  C <- NULL
  Model_ind <- NULL
  
  for (i in 1:M){
    D <- simu_xy(N)
    a0 <- sort(D$x)
    Sl <- lapply(X=a0, FUN=Rlc1, x=D$x, y=D$y)
    Su <- lapply(X=a0, FUN=Ruc1, x=D$x, y=D$y)
    Sl <- unlist(Sl)
    Su <- unlist(Su)
    min_num <- min(Sl,Su)
    min_ind <- which(pmin(Sl,Su)==min_num)
    a1 <- mean(a0[min_ind])
    model_ind <- as.numeric(  ( Rlc1(a1,D$x,D$y) < Ruc1(a1,D$x,D$y) ) )
    C <- c(C,a1)
    Model_ind <- c(Model_ind,model_ind)
  } 
  
  Con_err <- NULL
  for (i in 1:M){
    if (Model_ind[i]==0){
      cerr <- (1/2)*( 1-pnorm(C[i],0,1) ) + 
        (1/2)*(  pnorm(C[i],1,1/2) )
    }
    if (Model_ind[i]==1){
      cerr <- (1/2)*( pnorm(C[i],0,1) ) + 
        (1/2)*(  1-pnorm(C[i],1,1/2) )
    }
    Con_err <- c(Con_err,cerr)
  }
  out <- list(C=C, Model_ind=Model_ind, 
              Con_err=Con_err)
  return(out)
}


## N=100
Out100 <- classifier_3_11d(M,N1)
hist(Out100$C)
hist(Out100$Model_ind)
mean(Out100$Model_ind)
hist(Out100$Con_err)
## result for N=100
mean(Out100$Con_err)
#  0.2388344

## N=50
Out50 <- classifier_3_11d(M,N2)
hist(Out50$C)
hist(Out50$Model_ind)
mean(Out50$Model_ind)
hist(Out50$Con_err)
## result for N=50
mean(Out50$Con_err)
# 0.2444744



## 3.12

#a
g <- function(x){
  out <- -(3/4)*x^2 + 2*x - 1 + (1/2)*log(2)
  return(out)
}

x <- seq(-1,3.5,0.001)
plot(x,g(x),type="l")
abline(h=0,col="red")

# b
D3_12 <- read.table(file="/Users/apple/Desktop/ISU 2019 spring/STAT 602/hw/hw1/Problem3.12.txt",
                    header=TRUE, sep=",")
D3_12$y[which(D3_12$y==0)] <- -1

Risk <- function(b,x,y){
  x1 <- x - mean(x)
  out <- mean( exp(-y*(b[1] + b[2]*x1 + b[3]*x1^2)) )
  return(out)
}

fit2 <- nlm(f=Risk,p=c(1,0,-1),x=D3_12$x, y=D3_12$y)

fit2$estimate

f <- function(x,b,mu){
  out <- b[1] + b[2]*(x-mu) + b[3]*(x-mu)^2
  return(out)
}


# c
PRisk <- function(b,x,y,lambda){
  x1 <- x - mean(x)
  out <- mean( exp(-y*(b[1] + b[2]*x1 + b[3]*x1^2)) ) + lambda*b[3]^2
  return(out)
}

fit3_1 <- nlm(f=PRisk,p=c(0,0,0),x=D3_12$x, y=D3_12$y,lambda=0.1)
fit3_1$estimate
fit3_2 <- nlm(f=PRisk,p=c(0,0,0),x=D3_12$x, y=D3_12$y,lambda=0.01)
fit3_2$estimate
fit3_3 <- nlm(f=PRisk,p=c(0,0,0),x=D3_12$x, y=D3_12$y,lambda=0.001)
fit3_3$estimate

# plot
x <- seq(-3,5,0.1)
y2 <- f(x,b=fit2$estimate,mu=mean(D3_12$x))
y3_1 <- f(x,b=fit3_1$estimate,mu=mean(D3_12$x))
y3_2 <- f(x,b=fit3_2$estimate,mu=mean(D3_12$x))
y3_3 <- f(x,b=fit3_3$estimate,mu=mean(D3_12$x))

output3_12 <- data.frame(x=rep(x,5), y=c(g(x),y2,y3_1,y3_2,y3_3),
                         label=rep(c("y1","y2","y3_1","y3_2","y3_3"), each=length(x))
                         )



ggplot(data=output3_12) + 
  geom_line(aes(x=x, y=y, colour=label))



