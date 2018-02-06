#' induce.cfe
#' @description inducing ceiling/floor effects in data
#' @param floor.perc a (non-empty) numeric value from 0 to 1 denoting the desired percentage of floor effects
#' @param ceiling.perc a (non-empty) numeric value from 0 to 1 denoting the desired percentage of ceiling effects
#' @param y a (non-empty) numeric vector of data
#' @return y scores with induced ceiling/floor effects
#' @examples
#' x=rnorm(1000,0,1) #simulate "healthy data"
#' x.c20=induce.cfe(0,.2,x) #induce 20% ceiling effects into the data
#' sum(x.c20==max(x.c20))/length(x.c20) #check ceiling percentage
#' x.f20=induce.cfe(.2,0,x) #induce 20% floor effects into the data
#' sum(x.f20==min(x.f20))/length(x.f20) #check ceiling percentage
#' @export induce.cfe



induce.cfe<-function(floor.perc,ceiling.perc,y)
{
  m=mean(y)
  s=sd(y)
  a=qnorm(floor.perc,m,s)
  b=qnorm(1-ceiling.perc,m,s)
  y[y>=b]=b
  y[y<=a]=a
  return(y)
}
