#' Calculate the N required to estimate the ICC with a desired confidence interval
#'
#' Given a predicted ICC and \code{k} measures per individual/group, this
#' function will calculate the \code{N} individuals/groups required to obtain a
#' desired confidence interval \code{w}(Bonett 2002).
#'
#' More than one \code{ICC} or \code{k} may be given. In this case, the return
#' value is a dataframe with rows representing the values of the specified ICCs
#' and the columns yield the different \code{k} values.
#'
#' @param est.type A character string of either \code{"hypothetical"} indicating
#'   usage of the given values of \code{k} and \code{ICC} or if \code{"pilot"}
#'   is specified then to calculate these from the dataset provided. Just the
#'   first letter may be used.
#' @param w A \code{numeric} of desired width for the confidence interval about
#'   the ICC estimate.
#' @param ICC The expected intraclass correlation coefficient.
#' @param k The number of measurements per individual or group.
#' @param x A column name of \code{data} indicating the individual or group ID
#'   from a pilot study.
#' @param y A column name of \code{data} indicating the measurements from a pilot
#'   study.
#' @param data A \code{data.frame} from a pilot experiment.
#' @param alpha The alpha level to use when estimating the confidence interval.
#' @return \code{data.frame} indicating the N number of individuals or groups to
#'   use to estimate the given ICC with a desired confidence interval width.
#'   Rows represent different levels of ICC while columns indicate different
#'   levels of \code{k} measurements per individual/group.
#'
#' @author \email{matthewwolak@@gmail.com}
#' @seealso \code{\link{effort}}
#' @references D.G. Bonett. 2002. Statistics in Medicine, 21(9): 1331-1335.
#'
#' M.E. Wolak, D.J. Fairbairn, Y.R. Paulsen. 2011. Methods in Ecology and
#' Evolution, 3(1):129-137.
#' @examples
#'
#' # Example 1
#'   n1<-Nest("h", w = 0.14, ICC = 0.1, k = 10)
#'   n1
#' # Example 2
#'   data(ChickWeight)
#'   Nest("p", w = 0.14, x = Chick, y = weight, data = ChickWeight)
#'   ex2 <- ICCest(Chick, weight, ChickWeight)
#'   ex2$UpperCI - ex2$LowerCI #confidence interval width of pilot study
#'   ex2
#' # Example 3
#'   Nest("h", w = 0.14, ICC = seq(0.05, 0.15, 0.05), k = seq(10, 12, 1))
#'
#' @export
#' @importFrom stats qnorm
Nest<-function(est.type = c("hypothetical", "pilot"), w, ICC = NULL, k = NULL, x = NULL, y = NULL, data = NULL, alpha = 0.05){
  icall <- list(y = substitute(y), x = substitute(x))
  z <- qnorm(1-alpha/2, mean = 0, sd = 1)
  type <- match.arg(est.type)
  if(type == "hypothetical") {
    n.est <- matrix(nrow = length(ICC), ncol = length(k))   
    for (i in 1:length(ICC)){
      for(j in 1:length(k)){
        n.est1 <- 8*(z^2)*(((1-ICC[i])^2)*((1+(k[j]-1)*ICC[i])^2))/(k[j]*(k[j]-1)*(w^2))+1 
        n.est2 <- ceiling(n.est1)
        n.est[i,j] <- n.est2
      }
    }
    n.est.table <- data.frame(n.est, row.names = ICC)
    names(n.est.table) <- k
    return(n.est.table)
    } else {
         square<-function(z){z^2}
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
         ICC.results <- ICCest(x = x, y = y, data = tdata)
         n.est1b <- 8*(z^2)*(((1-ICC.results$ICC)^2)*((1+(ICC.results$k-1)*ICC.results$ICC)^2))/(ICC.results$k*(ICC.results$k-1)*(w^2))+1
         n.est2b <- ceiling(n.est1b)
         return(n.est2b)
      }
}
