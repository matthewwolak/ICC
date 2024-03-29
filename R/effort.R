#' Optimum \code{k} Measures Based Upon a Fixed Total Researcher Effort
#'
#' Given a fixed researcher effort (e.g., total number of assays able to be run),
#' this function plots the optimum \code{k} measurements per individual to use
#' in order to obtain the smallest confidence interval at an expected intraclass
#' correlation coefficient (ICC) estimate. The results are depicted graphically,
#' showing the tradeoff in confidence interval width with changing \code{k}.
#'
#' More than one \code{e} may be given. In this case, the graphical result
#' portrays multiple lines - each representing a different \code{e}.
#'
#' When \code{est.type="pilot"}, the function automatically generates an effort
#' 10 percent larger and smaller than the calculated effort from the pilot data.
#'
#' @param est.type A character string of either \code{"hypothetical"} indicating
#'   usage of the given values of effort (\code{e}) and intraclass correlation
#'   coefficient (\code{ICC}) or if \code{"pilot"} is specified then to
#'   calculate these from the dataset provided. Just the first letter may be
#'   used.
#' @param e A numeric value indicating the total effort (\code{n} individuals
#'   times \code{k} measurements per individual). May be a vector of effort
#'   levels.
#' @param ICC A numeric value of the expected intraclass correlation
#'   coefficient.
#' @param x Column name of \code{data} indicating the individual or group ID
#'   from a pilot study.
#' @param y Column name of \code{data} indicating the measurements from a pilot
#'   study.
#' @param data A \code{data.frame} from a pilot experiment.
#' @param alpha A numeric indicating the alpha level to use when estimating the
#'   confidence interval.
#'
#' @author \email{matthewwolak@@gmail.com}
#' @seealso \code{\link{Nest}}
#' @examples
#'
#' #Example 1
#'   effort(est.type = "h", e = c(30, 60, 120), ICC = 0.2)
#' #Example 2
#'   data(ChickWeight)
#'   effort(est.type = "p", x = Chick, y = weight, data = ChickWeight)
#'
#' @export
#' @importFrom stats qf
#' @importFrom graphics axis legend lines
effort <- function(est.type = c("hypothetical", "pilot"), e = NULL,
  ICC = NULL, x = NULL, y = NULL, data = NULL, alpha = 0.05){
  
  icall <- list(y = substitute(y), x = substitute(x))

  is.wholenumber<-function(x, tol = .Machine$double.eps^0.5) {
       abs(x - round(x)) < tol
       }

  CIW <- function(r, n, k){
       num.df <- n-1
       denom.df <- n*(k-1)
       F.stat <- (1-r+r*k)/(1-r)
       low.F <- qf(alpha/2, num.df, denom.df, lower.tail=FALSE)
       up.F <- qf(alpha/2, denom.df, num.df, lower.tail=FALSE)
       FL <- F.stat/low.F
       FU <- F.stat*up.F
       LowCI <- (FL-1)/(FL+k-1)
       UpCI <- (FU-1)/(FU+k-1)
     list(LowCI = LowCI, UpCI = UpCI, w = UpCI-LowCI)
     }

  k.maker <- function(effort, out = c("pairs", "widths")){
       pot.ens <- seq(3, effort/2, by=1)
       pot.ks <- effort/pot.ens
       ens <- sort(pot.ens, decreasing=FALSE)
       ks <- sort(pot.ks, decreasing=TRUE)
       typeb <- match.arg(out)
      if(typeb == "pairs"){ 
       pairs <- cbind(ks, ens)
      } else{
           ciw <- CIW(ICC, n=ens, k=ks)$w
        }
     }

  type <- match.arg(est.type)
    if(type == "hypothetical"){
      pairs <- sapply(e, k.maker, "p")
      widths <- sapply(e, k.maker, "w")
      }
    
    if(type == "pilot"){
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
      ICC <- ICCbare(x = x, y = y, data = tdata)
      EN <- dim(tdata)[1]
      e <- c(round(EN-EN*.15, 0), EN, round(EN+EN*.15, 0)) 
      pairs <- sapply(e, k.maker, "p")
      widths <- sapply(e, k.maker, "w")
    }

results <- list(e = e, pairs = pairs, widths = widths, ICC = ICC)

   line.types<-rep(c("dashed", "solid", "dotted"), length.out=length(e))
 plot(results$widths[[1]] ~results$pairs[[1]][,1], 
   main=paste("Optimal k;  ICC = ", round(results$ICC,2), sep=""),
   xlab="k", ylab="width of CI", axes=FALSE, type="n",
   xlim=c(1,25), ylim=c(0,1))
 axis(1, at=seq(2, 24, 2), cex.axis=1.1)
 axis(2, at=seq(0, 1, 0.1), cex.axis=1.1, las=1)
 for(i in 1:length(results$pairs)){
   lines(results$widths[[i]]~results$pairs[[i]][,1], lty=line.types[i]) 
   }
  ifelse(results$ICC>0.5 | mean(e)>400, placement<-"topright", placement<-"bottomright")
 legend(placement, legend=as.character(e), lty=line.types, title="Effort Level", inset=0.01)
}

