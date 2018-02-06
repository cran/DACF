#' rec.mean.var
#' @description recover mean and variance of the data with ceiling/floor effects
#' @param y a (non-empty) numeric vector of data with ceiling/floor effects
#' @return \item{ceiling.percentage}{the percentage of ceiling values in the data}
#'         \item{floor.percentage}{the percentage of floor values in the data}
#'         \item{est.mean}{estimated mean of the true scores}
#'         \item{est.var}{estimated variance of the true scores}
#' @examples
#' # simulate normally distributed true scores
#' x=rnorm(1000,2,4)
#' mean(x); var(x)
#' # induce 20% floor effects
#' # and estimate the true mean variance from the floor data
#' x.f=induce.cfe(.2,0,x)
#' rec.mean.var(x.f)
#' # induce 20% ceiling effects
#' # and estimate the true mean and variance from the ceiling data
#' x.c=induce.cfe(0,.2,x)
#' rec.mean.var(x.c)
#' # induce 20% and 10% of floor and ceiling effects, respectively
#' # and estimate the true mean and variance from the data with floor and ceiling effects
#' x.cf=induce.cfe(.2,.1,x)
#' rec.mean.var(x.cf)
#' @export rec.mean.var


rec.mean.var<-function(y){
  floor.perc=sum(y==min(y))/length(y)*(sum(y==min(y))>=1)
  ceiling.perc=sum(y==max(y))/length(y)*(sum(y==max(y))>=1)
  a=qnorm(floor.perc)
  b=qnorm(1-ceiling.perc)
  y.t=y[y!=max(y)&y!=min(y)]
  mean=mean(y.t)
  var=var(y.t)
  if (a==-Inf & b==Inf)
  {
    return(c(mean(y),var(y)))
  }
  else
  {
    if (a==-Inf & b!=Inf)
    {
      term1=b*dnorm(b)/(pnorm(b))
    }
    if (b==Inf & a!=-Inf)
    {
      term1=a*dnorm(a)/(pnorm(a))
    }
    if (a!=-Inf & b!=Inf)
    {
      term1=(b*dnorm(b)-a*dnorm(a))/(pnorm(b)-pnorm(a))
    }
    term2=(dnorm(b)-dnorm(a))/(pnorm(b)-pnorm(a))
    var.o=var/(1-term1-term2^2)
    mean.o=mean+sqrt(var.o)*term2
  }
  return(list(`ceiling.percentage`=ceiling.perc,`floor.percentage`=floor.perc,`est.mean`=mean.o,`est.var`=var.o))
}
