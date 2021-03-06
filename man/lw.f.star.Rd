% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lw.f.star.R
\name{lw.f.star}
\alias{lw.f.star}
\title{lw.f.star}
\usage{
lw.f.star(data, formula, method_type)
}
\arguments{
\item{data}{a dataframe of data with ceiling/floor effects and corresponding group variables in wide format}

\item{formula}{a formula denoting the dependent and independent variable, e.g., y~group}

\item{method_type}{a character string specifying the preferred method type. "a" uses the original sample size and "b" uses after-truncation sample size.}
}
\value{
\item{statistic}{the value of the Brown-Forsythe F star statistics}
        \item{p.value}{the p-value for the test}
        \item{est.f.squared}{effect size estimate in Cohen's f squared}
}
\description{
conduct an F star with for data with ceiling/floor effects
}
\examples{
dat=threeganova.sim(1000,.16,1)
dat[dat$group==1,3]=induce.cfe(0,.15,dat[dat$group==1,3])
lw.f.star(dat,y~group,"a") #using truncated n
lw.f.star(dat,y~group,"b") #using original n
}
