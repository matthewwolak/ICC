#' @rdname ICCest
#' @export
#' @importFrom stats aggregate
ICCbareF <- function(x, y, data = NULL){
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
  if(!is.factor(tdata[,2])){
    warning("x has been coerced to a factor")
    tdata[,2]<-as.factor(tdata[,2])
    }  
  tmp1 <- aggregate(tdata[, 1], list(tdata[, 2]),FUN = mean)
  tmp2 <- aggregate(tdata[, 1], list(tdata[, 2]), FUN = length)
  ord.data <- tdata[order(tdata[, 2]),]
  Treat.m <- rep(tmp1$x, tmp2$x)
  Among <- (Treat.m - rep(mean(tdata[, 1]), nrow(tdata)))^2
  Within <- (ord.data[, 1] - Treat.m)^2
  MS <- c(sum(Among), sum(Within)) / c(length(tmp2$x) - 1, length(tmp2$x) * (tmp2$x[1] - 1))
  var.a <- (MS[1] - MS[2]) / tmp2$x[1]
var.a / (var.a + MS[2])
}
