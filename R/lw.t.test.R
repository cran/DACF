#' lw.t.test
#' @description conduct a t test adjusting for ceiling and/or floor effects
#' @param x1 a (non-empty) numeric vector of data values for group 1 with floor/ceiling effects
#' @param x2 a (non-empty) numeric vector of data values for group 2 with floor/ceiling effects
#' @param method_type a character string specifying the preferred method type. "a" uses the original sample size and "b" uses after-truncation sample size.
#' @return \item{statistic}{the value of the adjusted t test statistics}
#' \item{p.value}{the p-value for the test}
#' \item{est.d}{effect size estimate as in Cohen's d}
#' \item{conf.int}{95\% confidence interval}
#' @examples
#' x1.c=induce.cfe(0,.3,rnorm(1000,20,5)) #group 1 scores with 30% ceiling data
#' x2.c=induce.cfe(.15,0,rnorm(1000,30,5)) #group 2 scores with 15% floor data
#' lw.t.test(x1.c,x2.c,"a") #using truncated n
#' lw.t.test(x1.c,x2.c,"b") #using original n
#' @export lw.t.test
#' @import stats

lw.t.test<-function(x1,x2,method_type){
  mv1=rec.mean.var(x1)
  mv2=rec.mean.var(x2)
  m1=mv1[[3]]
  v1=mv1[[4]]
  m2=mv2[[3]]
  v2=mv2[[4]]
  n=length(x1)
  if (method_type=="a"){
    n1=n-sum(x1==max(x1)|x1==min(x1))*(sum(x1==max(x1)|x1==min(x1))>2)
    n2=n-sum(x2==max(x2)|x1==min(x2))*(sum(x2==max(x2)|x1==min(x2))>2)
  }
  if (method_type=="b"){n1=n;n2=n}
  if (method_type!="a"&method_type!="b"){stop("method type not recognized")}
  t=(m1-m2)/sqrt(v1/n1+v2/n2)
  stderrx <- sqrt(v1/n1)
  stderry <- sqrt(v2/n2)
  stderr <- sqrt(stderrx^2 + stderry^2)
  df <- stderr^4/(stderrx^4/(n1 - 1) + stderry^4/(n2 - 1))/2-1
  df=ifelse(df>0,df,1)
  p=2*pt(-abs(t),df)
  d=(m1-m2)/sqrt((n1*v1+n2*v2)/(n1+n2))
  ll=(m1-m2)-qt(.975,df)*stderr
  ul=(m1-m2)+qt(.975,df)*stderr
  return(list('statistic'=t,'p.value'=p,'est.d'=d,'conf.int'=c(ll,ul)))
}
