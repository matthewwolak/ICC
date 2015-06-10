\name{ICCbare}
\alias{ICCbare}
\title{Simple function to estimate the Intraclass Correlation Coefficient}
\description{This function estimates the Intraclass Correlation Coefficient (ICC).  It is meant to be as simple and fast as possible for use in Monte Carlo simulations or bootstrapping.  If the design is balanced, it will calculate variance components 'by hand', instead of using the aov() function.}
\usage{
ICCbare(x, y, data)
}
\arguments{
  \item{x}{column name indicating individual or group id in the dataframe \code{data}}
  \item{y}{column name indicating measurements in the dataframe \code{data}. Each entry in \code{x} must have at least one non-NA value in \code{y}}
  \item{data}{a dataframe containing \code{x} and \code{y}}
}

\details{\code{ICCbare} can be used on balanced or unbalanced datasets with NAs.  ICCbareF is similar, however ICCbareF should not be used with unbalanced datasets.
}

\value{
  \item{ICC}{the intraclass correlation coefficient}

}
\author{Matthew Wolak \email{matthewwolak@gmail.com}}
\seealso{\code{\link{ICCest}}, \code{\link{ICCbareF}}}
