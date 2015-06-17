\name{ICCbareF}
\alias{ICCbareF}
\title{Simple Estimation of the Intraclass Correlation Coefficient}
\description{Estimates the Intraclass Correlation Coefficient (ICC) and is meant to be as simple and fast as possible for use in Monte Carlo simulations or bootstrapping. Calculates the variance components 'by hand', instead of using the aov() function.}

\usage{
ICCbareF(x, y, data)
}
\arguments{
  \item{x }{column name indicating individual or group id in the dataframe \code{data}}
  \item{y }{column name indicating measurements in the dataframe \code{data}. Each entry in \code{x} must have at least one non-NA value in \code{y}}
  \item{data }{a dataframe containing \code{x} and \code{y}}
}

\details{\code{ICCbareF} is distinguished from \code{ICCbare}, in that \code{ICCbare} is more flexible and can handle missing values and unbalanced datasets. \code{ICCbareF} cannot and should only be used on balanced datasets without any NAs.
}

\value{
  \item{ICC }{the intraclass correlation coefficient}

}
\author{Matthew Wolak \email{matthewwolak@gmail.com}}
\seealso{\code{\link{ICCest}}, \code{\link{ICCbare}}
}

