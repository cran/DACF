#' lw.f.star
#' @description conduct an F star with for data with ceiling/floor effects
#' @param data a dataframe of data with ceiling/floor effects and corresponding group variables in wide format
#' @param formula a formula denoting the dependent and independent variable, e.g., y~group
#' @param method_type a character string specifying the preferred method type. "a" uses the original sample size and "b" uses after-truncation sample size.
#' @return \item{statistic}{the value of the Brown-Forsythe F star statistics}
#'         \item{p.value}{the p-value for the test}
#'         \item{est.f.squared}{effect size estimate in Cohen's f squared}
#' @examples
#' dat=threeganova.sim(1000,.16,1)
#' dat[dat$group==1,3]=induce.cfe(0,.15,dat[dat$group==1,3])
#' lw.f.star(dat,y~group,"a") #using truncated n
#' lw.f.star(dat,y~group,"b") #using original n
#' @export lw.f.star

lw.f.star<-function(data,formula,method_type){
  mf <- model.frame(formula=formula, data=data)
  iv <- model.matrix(attr(mf, "terms"), data=mf)[,2]
  dv <- model.response(mf)
  samp_per_group=as.numeric(table(iv))
  mvs=matrix(unlist(by(dv,iv,rec.mean.var)),ncol=length(samp_per_group))
  samp_means_per_group=mvs[3,]
  samp_var_per_group=mvs[4,]
  if (method_type=="a"){
    f.star.test(samp_means_per_group,samp_var_per_group,samp_per_group)
    } else {
    samp_per_group=(1-mvs[1,]-mvs[2,])*samp_per_group
    f.star.test(samp_means_per_group,samp_var_per_group,samp_per_group)
  }
}

