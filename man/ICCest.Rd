\name{ICCest}
\alias{ICCest}
\title{Estimate the Intraclass Correlation Coefficient}
\description{Estimates the ICC and confidence intervals using the variance components from a one-way ANOVA.}
\usage{
ICCest(x, y, data = NULL, alpha = 0.05, CI.type = c("THD", "Smith"))
}
\arguments{
  \item{x }{column name indicating individual or group id in the dataframe \code{data}}
  \item{y }{column name indicating measurements in the dataframe \code{data}}
  \item{data }{a dataframe containing \code{x} and \code{y}}
  \item{alpha }{the alpha level to use when estimating the confidence interval.  Default is 0.05.}
  \item{CI.type }{the particular confidence interval to estimate. Can be specified by just the first letter of the name. See Details section for more.}
}

\details{If the dependent variable, \code{x}, is not a factor, then the function will change it into a factor and produce a warning message. 

The confidence interval can be estimated from one of two methods included here. CIs of the type \code{"THD"} are based upon the exact confidence limit equation in Searle (1971) and can be used for unbalanced data (see Thomas & Hultquist 1978; Donner 1979).

CIs of the type \code{"Smith"} are based upon the approximate formulas for the standard error of the ICC estimate (Smith 1956).
}

\value{
  \item{ICC }{the intraclass correlation coefficient}
  \item{LowerCI }{the lower confidence interval limit, where the confidence level is set by \code{alpha}}
  \item{UpperCI }{the upper confidence interval limit, where the confidence level is set by \code{alpha}}
  \item{N }{the total number of individuals or groups used in the analysis}
  \item{k }{the number of measurements per individual or group. In an unbalanced design, k is always less than the mean number of measurements per individual/group and is calculated using the equation in Lessells and Boag (1987).}
  \item{varw }{the within individual or group variance}
  \item{vara }{the among individual or group variance}

}
\references{C.M. Lessells and P.T. Boag. 1987. The Auk, 104(1):116-121.
Searle, S.R. 1971. Linear Models. New York: Wiley.
Thomas, J.D. and Hultquist, R.A. 1978. Annals of Statistics, 6:582-587.
Donner, A. 1979. American Journal of Epidemiology, 110:335-342.
Smith, C.A.B. 1956. Annals of Human Genetics, 21:363-373.
}
\author{Matthew Wolak \email{matthewwolak@gmail.com}}

\seealso{\code{\link{ICCbare}}}
\examples{
data(ChickWeight)
ICCest(Chick, weight, data = ChickWeight, CI.type = "S")
}
