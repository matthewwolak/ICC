\name{effort}
\alias{effort}
\title{Plots the optimum \code{k} measures per individual (or group), based upon a fixed total researcher effort.
}

\description{Given a fixed researcher effort (e.g., total number of assays able to be run), this function plots the optimum \code{k} measurements per individual to use in order to obtain the smallest confidence interval at an expected intraclass correlation coefficient (ICC) estimate.  The results are depicted graphically, showing the tradeoff in confidence interval width with changing \code{k}.
}

\usage{
effort(est.type = c("hypothetical", "pilot"), e = NULL, ICC = NULL,
x = NULL, y = NULL, data = NULL, alpha = 0.05)
}

\arguments{
  \item{est.type}{character string of either \code{"hypothetical"} indicating usage of the given values of effort (\code{e}) and intraclass correlation coefficient (\code{ICC}) or if \code{"pilot"} is specified then to calculate these from the dataset provided.  Just the first letter may be used.
}
  \item{e}{the total effort (\code{n} individuals times \code{k} measurements per individual).  May be a vector of effort levels.
}
  \item{ICC}{expected intraclass correlation coefficient}
  \item{x}{column name of \code{data} indicating the individual or group ID from a pilot study}
  \item{y}{column name of \code{data} indicating the measurements from a pilot study}
  \item{data}{a \code{data.frame} from a pilot experiment}
  \item{alpha}{the alpha level to use when estimating the confidence interval}
}

\details{More than one \code{e} may be given.  In this case, the graphical result portrays multiple lines - each representing a different \code{e}

When \code{est.type="pilot"}, the function automatically generates an effort 10 percent larger and smaller than the calculated effort from the pilot data.
}

\author{Matthew Wolak \email{matthewwolak@gmail.com}
}

\seealso{\code{\link{Nest}}
}

\examples{
#Example 1
effort(est.type="h", e=c(30, 60, 120), ICC=0.2)

#Example 2
data(ChickWeight)
effort(est.type="p", x=Chick, y=weight, data=ChickWeight)
}

%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }

