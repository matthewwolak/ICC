#' Estimate the Intraclass Correlation Coefficient (ICC)
#'
#' Estimates the ICC and confidence intervals using the variance components from
#' a one-way ANOVA.
#'
#' \code{ICCbare} conducts simple estimation of the ICC that is meant to be as
#' simple as possible and fast for use in Monte Carlo simulations or
#' bootstrapping. If the design is balanced, \code{ICCbare} will calculate
#' variance components 'by hand', instead of using the \code{aov} function.
#' \code{ICCbare} can be used on balanced or unbalanced datasets with NAs.
#'
#' \code{ICCbareF} is similar to \code{ICCbare}, however \code{ICCbareF} should
#' not be used with unbalanced datasets. \code{ICCbareF} is distinguished from
#' \code{ICCbare}, in that \code{ICCbare} is more flexible and can handle
#' missing values and unbalanced datasets.
#'
#' If the dependent variable, \code{x}, is not a factor, then the function will
#' change it into a factor and produce a warning message. 
#'
#' For \code{ICCest} he confidence interval (CI) can be estimated from one of
#' two methods included here. CIs of the type \code{"THD"} are based upon the
#' exact confidence limit equation in Searle (1971) and can be used for
#' unbalanced data (see Thomas and Hultquist 1978; Donner 1979). CIs of the type
#' \code{"Smith"} are based upon the approximate formulas for the standard error
#' of the ICC estimate (Smith 1956).
#'
#' @aliases ICCbare ICCbareF
#' @param x A column name indicating individual or group id in the dataframe
#'   \code{data}.
#' @param y A column name indicating measurements in the dataframe \code{data}.
#' @param data A data.frame containing \code{x} and \code{y}.
#' @param alpha A numeric specifying the alpha level to use when estimating the
#'   confidence interval. Default is 0.05.
#' @param CI.type A character indicating the particular confidence interval to
#'   estimate. Can be specified by just the first letter of the name. See
#'   Details section for more.
#'
#' @return a \code{list}:
#'   \describe{
#'     \item{ICC }{the intraclass correlation coefficient}
#'     \item{LowerCI }{the lower confidence interval limit, where the confidence
#'       level is set by \code{alpha}}
#'     \item{UpperCI }{the upper confidence interval limit, where the confidence
#'       level is set by \code{alpha}}
#'     \item{N }{the total number of individuals or groups used in the analysis}
#'     \item{k }{the number of measurements per individual or group. In an
#'       unbalanced design, k is always less than the mean number of
#'       measurements per individual or group and is calculated using the
#'       equation in Lessells and Boag (1987).}
#'     \item{varw }{the within individual or group variance}
#'     \item{vara }{the among individual or group variance}
#'   } 
#' @author \email{matthewwolak@@gmail.com}
#' @references C.M. Lessells and P.T. Boag. 1987. The Auk, 104(1):116-121.
#'
#' Searle, S.R. 1971. Linear Models. New York: Wiley.
#'
#' Thomas, J.D. and Hultquist, R.A. 1978. Annals of Statistics, 6:582-587.
#'
#' Donner, A. 1979. American Journal of Epidemiology, 110:335-342.
#'
#' Smith, C.A.B. 1956. Annals of Human Genetics, 21:363-373.
#' @examples
#' 
#' data(ChickWeight)
#' # ICCest
#'   ICCest(Chick, weight, data = ChickWeight, CI.type = "S")
#' @export
#' @importFrom stats aggregate anova aov na.omit qf qnorm
ICCest <- function(x, y, data = NULL, alpha = 0.05, CI.type = c("THD", "Smith")){
  square <- function(z){z^2}
  icall <- list(y = substitute(y), x = substitute(x))

  if(is.character(icall$y)){
    warning("passing a character string to 'y' is deprecated since ICC vesion 2.3.0 and will not be supported in future versions. The argument to 'y' should either be an unquoted column name of 'data' or an object")
    if(missing(data)) stop("Supply either the unquoted name of the object containing 'y' or supply both 'data' and then 'y' as an unquoted column name to 'data'")
    icall$y <- eval(as.name(y), data, parent.frame())
  } 
  if(is.name(icall$y)) icall$y <- eval(icall$y, data, parent.frame())
  if(is.call(icall$y)) icall$y <- eval(icall$y, data, parent.frame())
  if(is.character(icall$y)) icall$y <- eval(as.name(icall$y), data, parent.frame())


  if(is.character(icall$x)){
    warning("passing a character string to 'x' is deprecated since ICC vesion 2.3.0 and will not be supported in future versions. The argument to 'x' should either be an unquoted column name of 'data' or an object")
    if(missing(data)) stop("Supply either the unquoted name of the object containing 'x' or supply both 'data' and then 'x' as an unquoted column name to 'data'")
    icall$x <- eval(as.name(x), data, parent.frame())
  } 
  if(is.name(icall$x)) icall$x <- eval(icall$x, data, parent.frame())
  if(is.call(icall$x)) icall$x <- eval(icall$x, data, parent.frame())
  if(is.character(icall$x) && length(icall$x) == 1) icall$x <- eval(as.name(icall$x), data, parent.frame())


  tdata <- data.frame(icall)
  tdata <- na.omit(tdata)
  a <- length(unique(tdata$x))

  if(!is.null(attributes(tdata)$na.action)){
     warning(cat("NAs removed from rows:\n", unclass(attributes(tdata)$na.action), "\n"))
  } 
  if(!is.factor(tdata$x)){
     warning("'x' has been coerced to a factor")
     tdata$x <- as.factor(tdata$x)
  } else{
       if(length(levels(tdata$x)) > a){
          tdata$x <- factor(as.character(tdata$x), levels = unique(tdata$x))
          warning("Missing levels of 'x' have been removed")
       } 
    } 

  tmpbb <- anova(aov(y ~ x, data = tdata))
  num.df <- tmpbb$Df[1]
  denom.df <- tmpbb$Df[2]
  MSa <- tmpbb$'Mean Sq'[1]
  MSw <- var.w <- tmpbb$'Mean Sq'[2]
  tmp.outj <- aggregate(y ~ x, data = tdata, FUN = length)$y
  k <- (1/(a-1)) * (sum(tmp.outj) - (sum(square(tmp.outj)) / sum(tmp.outj)))
  var.a <- (MSa - MSw) / k
  r <- var.a / (var.w + var.a)

  low.F <- qf(alpha/2, num.df, denom.df, lower.tail = FALSE)
  N <- nrow(tdata)
  n.bar <- N/a
  n.not <- n.bar - sum(square(tmp.outj - n.bar) / ((a - 1) * N))	
    type <- match.arg(CI.type)
      if(type == "THD"){
	up.F <- qf(alpha/2, denom.df, num.df, lower.tail = FALSE)	
	FL <- (MSa/MSw) / low.F
	FU <- (MSa/MSw) * up.F
	low.CI <- (FL - 1) / (FL + n.not - 1)
	up.CI <- (FU - 1) / (FU + n.not - 1)
       }
      if(type == "Smith"){
	z.not <- qnorm(alpha/2)
	Vr <- (2*square(1-r) / square(n.not)) * ((square((1+r*(n.not-1))) / (N-a)) + ((a-1)*(1-r)*(1+r*(2*n.not-1))+square(r)*(sum(square(tmp.outj))-2*(1/N)*sum((tmp.outj^3))+(1/square(N))*square(sum(square(tmp.outj)))))/ square(a-1))
	low.CI <- r + z.not * sqrt(Vr)
	up.CI <- r - z.not * sqrt(Vr) 
      }

list(ICC = r, LowerCI = low.CI, UpperCI = up.CI, N = a, k = k, varw = var.w, vara = var.a)
}

