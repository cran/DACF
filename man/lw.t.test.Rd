% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lw.t.test.R
\name{lw.t.test}
\alias{lw.t.test}
\title{lw.t.test}
\usage{
lw.t.test(x1, x2, method_type)
}
\arguments{
\item{x1}{a (non-empty) numeric vector of data values for group 1 with floor/ceiling effects}

\item{x2}{a (non-empty) numeric vector of data values for group 2 with floor/ceiling effects}

\item{method_type}{a character string specifying the preferred method type. "a" uses the original sample size and "b" uses after-truncation sample size.}
}
\value{
\item{statistic}{the value of the adjusted t test statistics}
\item{p.value}{the p-value for the test}
\item{est.d}{effect size estimate as in Cohen's d}
\item{conf.int}{95\% confidence interval}
}
\description{
conduct a t test adjusting for ceiling and/or floor effects
}
\examples{
x1.c=induce.cfe(0,.3,rnorm(1000,20,5)) #group 1 scores with 30\% ceiling data
x2.c=induce.cfe(.15,0,rnorm(1000,30,5)) #group 2 scores with 15\% floor data
lw.t.test(x1.c,x2.c,"a") #using truncated n
lw.t.test(x1.c,x2.c,"b") #using original n
}
