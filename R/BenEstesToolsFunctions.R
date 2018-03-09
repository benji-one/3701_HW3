# --- Quiz1 Functions ---

#' Finds the mean, variance, and sd for a vector
#' @param x a Vector
#' @return A list of mean, variance, and sd
#' @example
#'
#' x <- scan(url("http://www.stat.umn.edu/geyer/3701/data/q1p1.txt"))
#' func1(x)

func1 <- function(x){

  a <- (1/length(x))*sum(x)
  b <- (1/length(x))*sum((x-a)^2)
  c <- sqrt(b)

  ans <- list(mean=a,var=b,sd=c)
  return(ans)
}

#' Finds the mean, variance, and sd for a vector, has a GIEMO check as well
#' @param x a Vector
#' @return A list of mean, variance, and sd, or an error.
#' @example
#'
#' x <- scan(url("http://www.stat.umn.edu/geyer/3701/data/q1p1.txt"))
#' func1(x)

func2 <- function(x){

  for(i in 1:length(x))
  {
    if(is.finite(x[i])==FALSE)
    {
      return("Error: Non-numeric input")
    }
  }

  a <- (1/length(x))*sum(x)
  b <- (1/length(x))*sum((x-a)^2)
  c <- sqrt(b)

  ans <- list(mean=a,var=b,sd=c)
  return(ans)
}

#' Finds the mle for a dataset given a gamma distribution
#'@param x a vector
#'@return the mle for x
#'@example
#'
#'x <- scan(url("http://www.stat.umn.edu/geyer/3701/data/q1p3.txt"))
#'mle1(x)

mle1 <- function(x)
{
  alpha <- pi
  interval <- mean(x)+c(-1,1)*3*sd(x)
  interval <- pmax(mean(x)/1e3,interval)

  log1 <- function(alpha,x)
  {
    sum(dgamma(x,shape=alpha,log=TRUE))
  }

  return(optimize(log1,maximum = TRUE, interval,x = x)$maximum)
}




# --- HW1 Functions ---

#' Finds the mean, variance, and sd for a vector with an uneven probability distribution.
#' @param x a Vector
#' @param p a probability distribution
#' @return A list of mean, variance, and sd
#' @example
#'
#' d <- read.table(url("http://www.stat.umn.edu/geyer/3701/data/q1p4.txt"),
#' header = TRUE)
#' func4(d$x,d$p)

func4 <- function(x,p){

  a <- sum(x*p)
  b <- sum(p*(x-a)^2)
  c <- sqrt(b)

  ans <- list(mean=a,var=b,sd=c)
  return(ans)
}


#' Finds the mean, variance, and sd for a vector with an uneven probability distribution, with a GIEMO check
#' @param x a Vector
#' @param p a probability distribution
#' @return A list of mean, variance, and sd, or an error.
#' @example
#'
#' d <- read.table(url("http://www.stat.umn.edu/geyer/3701/data/q1p4.txt"),
#' header = TRUE)
#' func4(d$x,d$p)

func5 <- function(x,p){

  for(i in 1:length(x))
  {
    if((is.finite(x[i])==FALSE)|(is.finite(p[i])==FALSE)|(isTRUE(all.equal(sum(p),1))==FALSE)|((p[i]<0)==TRUE))
    {
      return("Error: Non-standard inputs")
    }
  }

  a <- sum(x*p)
  b <- sum(p*(x-a)^2)
  c <- sqrt(b)

  ans <- list(mean=a,var=b,sd=c)
  return(ans)
}

#' Finds the MLE for a generic data distribution.
#'
#'@param x A data set
#'@param interval A vector interval
#'@param FUN A distribution function
#'@return The MLE of the parameter theta
#'
#'@example
#'gamma.FUN <- function(theta, x) dgamma(x, shape = theta, log = TRUE)
#'cauchy.FUN <- function(theta, x) dcauchy(x, location = theta, log = TRUE)
#'binom.FUN <- function(theta, x) dbinom(x, 20, prob = 1 / (1 + exp(- theta)), log = TRUE)
#'
#'xgam <- load("data/xgam.rda")
#'xcau <- load("data/xcau.rda")
#'xbin <- load("data/xbin.rda")
#'interval <- 1:100
#'
#'mle2(xgam,interval,gamma.FUN)
#'mle2(xcau,interval,cauchy.FUN)
#'mle2(xbin,interval,binom.FUN)

mle2 <- function(x,interval,FUN)
{
  theta <- pi

  log1 <- function(theta,x)
  {
    sum(FUN(theta,x))
  }

  return(optimize(log1,maximum = TRUE, interval,x=x)$maximum)
}

# --- Quiz2 Functions ---

#' A matrix calculator that calculates x^t(A^-1)x, has GIEMO checks for non-numeric and non compatible dims.
#' @param x a vector
#' @param A a matrix
#'
#' @exmaple
#' load(url("http://www.stat.umn.edu/geyer/3701/data/q2p1.rda"))
#' matCalc(a,x)

matCalc <- function(A,x)
{
  if((sum(is.finite(A))!=sum(is.finite(A)|TRUE))|(sum(is.finite(x))!=sum(is.finite(x)|TRUE)))
  {
    return("Non numeric arguments")
  }

  if((ncol(A)!=nrow(A))|(length(x)!=nrow(A)))
  {
    return("Non-compatible dimmensions")
  }

  A_i <- solve(A)
  result <- x%*%A_i%*%x
  return(result)
}


#' A matrix calculator that calculates x^t(A^-1)x, has GIEMO checks for non-numeric and non compatible dims.
#' Acts as an operator rather than a function
#' @param x a vector
#' @param A a matrix
#'
#' @exmaple
#' load(url("http://www.stat.umn.edu/geyer/3701/data/q2p1.rda"))
#' a%matCalcOp%x

"%matCalcOp%" <- function(A,x)
{
  if((sum(is.finite(A))!=sum(is.finite(A)|TRUE))|(sum(is.finite(x))!=sum(is.finite(x)|TRUE)))
  {
    return("Non numeric arguments")
  }

  if((ncol(A)!=nrow(A))|(length(x)!=nrow(A)))
  {
    return("Non-compatible dimmensions")
  }

  A_i <- solve(A)
  result <- x%*%A_i%*%x
  return(result)
}

#' Standardizes all the columns of a matrix
#' @param A, a matrix
#'
#' @example
#' load(url("http://www.stat.umn.edu/geyer/3701/data/q2p3.rda"))
#' mat.Standard(a)

mat.Standard <- function(A)
{
  if(nrow(A)==1)
  {
    return("Matrix with one row cannot be standardized")
  }

  for (i in 1:ncol(A))
  {
    A[,i] <- (A[,i]-mean(A[,i]))/sd(A[,i])
  }

  return(A)
}



# --- HW2 Functions ---

#' A function that standardizes a single vector, designed for use in the apply function. Also has GIEMO
#' @param v a vector
#'
#' @example
#' load(url("http://www.stat.umn.edu/geyer/3701/data/q2p3.rda"))
#' apply(a,2,vect.Standardize)

vect.Standardize <- function(v)
{
  if(length(v)==1){return("Vectors of length one cannot be standardized")}
  (v-mean(v))/sd(v)
}


#' A rudimentary version of the apply function.
#' @param x a vector or matrix
#' @param MARGIN an integer 1 or 2 (row or column)
#' @param FUN a function
#' @param ... a catch all that passes any additional arguments to FUN
#'
#' @example
#' fred <- array(1:6,c(3,2))
#' apply(fred,1,mean)
#' myapply(fred,1,mean)

myapply <- function(X,MARGIN,FUN,...)
{
  FUN <- match.fun(FUN)

  if(MARGIN!=1&&MARGIN!=2)
  {
    return("Error: MARGIN must be 1 or 2")
  }

  if(MARGIN==1)
  {
    new <- FUN(X[1,],...)
    for (i in 2:nrow(X)) {new <-cbind(new,FUN(X[i,],...))}
  }

  if(MARGIN==2)
  {
    new <- FUN(X[,1],...)
    for (i in 2:ncol(X)) {new <- cbind(new,FUN(X[,i],...))}
  }

  return(new)
}

# --- Quiz3 Functions ---

#' Filters housing data by state
#'@param x Housing data
#'@param STATE State name two letter abbreviation
#'

filterState <- function(x,STATE)
{
  x %>% filter(State=="STATE")
}


#' Basic wrapper function for ggplot
#'@param x <- vector
#'

plotMyData<-function(x)
{
  x%>% ggplot2::ggplot()+ggplot2::aes(x=x, y=p)+ggplot2::geom_point()
}


