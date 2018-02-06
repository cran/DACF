#' threeganova.sim
#' @description simulate three-group anova data
#' @param group_n a (non-empty) numeric value of desired sample size per group
#' @param f_sqr a (non-empty) numeric value of desired Cohen's f squared value
#' @param sd.1 a (non-empty) numeric value of desired standard deviation ratio
#' @return a dataframe containing scores "y", grouping factor "group", and residual errors.
#' @examples
#' sample.3g=threeganova.sim(1000,.16,5) #data of n=1000, sd1=sd3=1 and sd2=5, and f^2=.16
#' colnames(sample.3g) #examine the column names
#' dim(sample.3g) #examine the data structure
#' aggregate(sample.3g$y,sd,by=list(sample.3g$group)) #check group standard deviations
#' @export threeganova.sim



threeganova.sim<-function(group_n,f_sqr,sd.1)
{
  alpha=sqrt(f_sqr*3/2*(sd.1^2+1+1)/3)
  temp_1=NULL
  temp_2=NULL
  temp_3=NULL
  while (min(sum(temp_1<=qnorm(.9)),sum(temp_1<=qnorm(.8)),sum(temp_1<=qnorm(.7)),sum(temp_1<=qnorm(.6)),sum(temp_2<=qnorm(.9)),sum(temp_2<=qnorm(.8)),sum(temp_2<=qnorm(.7)),sum(temp_2<=qnorm(.6)),sum(temp_3<=qnorm(.9)),sum(temp_3<=qnorm(.8)),sum(temp_3<=qnorm(.7)),sum(temp_3<=qnorm(.6)))<=3)
  {
    e1=rnorm(group_n,0,sd.1)
    e2=rnorm(group_n,0,1)
    e3=rnorm(group_n,0,1)
    temp_1=alpha+e1
    temp_2=e2
    temp_3=-alpha+e3
  }
  y_temp=c(temp_1,temp_2,temp_3)
  y=matrix(c(c(e1,e2,e3),rep(c(2,1,3),each=group_n),y_temp),ncol=3)
  y=data.frame(y)
  colnames(y)=c("e","group","y")
  return(y)
}
