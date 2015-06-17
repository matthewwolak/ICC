\name{Nest}
\alias{Nest}
\title{Calculate the N individuals/groups required to estimate the ICC with a desired confidence interval}
\description{Given a predicted ICC and \code{k} measures per individual/group, this function will calculate the \code{N} individuals/groups required to obtain a desired confidence interval \code{w}(according to Bonett, 2002).
}
\usage{
Nest(est.type = c("hypothetical", "pilot"), w, ICC = NULL, k = NULL,
x = NULL, y = NULL, data = NULL, alpha = 0.05)
}
\arguments{
  \item{est.type }{character string of either \code{"hypothetical"} indicating usage of the given values of \code{k} and \code{ICC} or if \code{"pilot"} is specified then to calculate these from the dataset provided. Just the first letter may be used
}
  \item{w }{desired width of the confidence interval about the ICC estimate}
  \item{ICC }{expected intraclass correlation coefficient}
  \item{k }{number of measurements per individual or group}
  \item{x }{column name of \code{data} indicating the individual or group ID from a pilot study}
  \item{y }{column name of \code{data} indicating the measurements from a pilot study}
  \item{data }{a \code{data.frame} from a pilot experiment}
  \item{alpha }{the alpha level to use when estimating the confidence interval}
}
\details{More than one \code{ICC} or \code{k} may be given. In this case, the return value is a dataframe with rows representing the values of the specified ICCs and the columns yield the different \code{k} values.
}
\value{\code{data.frame} indicating the N number of individuals or groups to use to estimate the given ICC with a desired confidence interval width. Rows represent different levels of ICC while columns indicate different levels of \code{k} measurements per individual/group.}
\references{D.G. Bonett. 2002. Statistics in Medicine, 21(9): 1331-1335.

M.E. Wolak, D.J. Fairbairn, Y.R. Paulsen. 2011. Methods in Ecology and Evolution.}
\author{Matthew Wolak \email{matthewwolak@gmail.com}
}
\seealso{\code{\link{ICCest}}
}
\examples{
#Example 1
  n1<-Nest("h", w = 0.14, ICC = 0.1, k = 10)
  n1

#Example 2
  data(ChickWeight)
  Nest("p", w = 0.14, x = Chick, y = weight, data = ChickWeight)
  ex2 <- ICCest(Chick, weight, ChickWeight)
  ex2$UpperCI - ex2$LowerCI #confidence interval width of pilot study
  ex2

#Example 3
  Nest("h", w = 0.14, ICC = seq(0.05, 0.15, 0.05), k = seq(10, 12, 1))
}
