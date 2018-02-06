#' f.star.test
#' @description conduct a Brown-Forsythe F star test
#' @param means a (non-empty) numeric vector of the group means
#' @param variances a (non-empty) numeric vector of the group variances
#' @param ns a (non-empty) numeric vector of sample sizes per group
#' @return \item{statistic}{the value of the adjusted Brown-Forsythe F star statistic}
#'         \item{p.value}{the p-value for the test}
#'         \item{est.f.squared}{effect size estimate as in Cohen's f squared}
#' @examples
#' # a f star test for three-group mean comparison
#' f.star.test(c(-.2,0,.2),c(1,1,1),c(100,100,100))
#' f.star.test(c(0,0,1),c(2,1,3),c(100,100,100))
#' @export f.star.test


f.star.test<-function(means,variances,ns){
    MSB.s=sum(ns*(means-mean(means))^2)
    MSW.s=sum((1-ns/sum(ns))*variances)
    F.s=MSB.s/MSW.s
    gs=((1-ns/sum(ns))*variances)/MSW.s
    f=1/(sum(gs^2/(ns-1)))
    p.value=1-pf(F.s,length(means)-1,f)
    f.squared=(length(means)-1)*F.s/sum(ns)
    return(list("statistic"=F.s,"p.value"=p.value,"est.f.squared"=f.squared))
}
