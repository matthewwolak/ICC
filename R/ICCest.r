ICCest<-function(x, y, data=data, alpha=0.05, CI.type=c("THD", "Smith"))
{
  if(!is.data.frame(data))
       stop("object dataframe is of the type '", class(data), "' and must be of type 'data.frame'")
  square<-function(z){z^2}
  ICCcall <- Call <-match.call()
  xc<-as.character(ICCcall[[2L]])
  yc<-as.character(ICCcall[[3L]])
  inds<-unique(data[xc])[[1]]
  a<-length(inds)
  tdata<-data.frame(data[yc], data[xc])
  if(!is.factor(tdata[,2])){
    warning("x has been coerced to a factor")
    tdata[,2]<-as.factor(tdata[,2])
    }
  if(length(levels(tdata[,2])) > length(unique(tdata[,2])))
	stop("levels assigned to 'x' are greater than the actual levels of 'x'")
  
  nacheck <- function(x){
     result <- unlist(lapply(x, FUN = is.na))
     if(all(result)){
     return(FALSE)
       } else{return(TRUE)}
  } 
  
  unstackedt <- unstack(tdata)
  TF <- lapply(unstackedt, FUN = nacheck)
  if(any(unlist(TF) == FALSE)){
	warning("one or more groups in 'x' do not contain any records and have been removed")
	tdata <- tdata[!tdata[,2] == which(unlist(TF) == FALSE), ]
  }

  tmpbb<-anova(aov(tdata[,1]~tdata[,2], data=tdata))
  num.df<-tmpbb[1][1,1]
  denom.df<-tmpbb[1][2,1];
  MSa<-tmpbb[3][1,1]
  MSw<-tmpbb[3][2,1]
  
  tmp.outj<-data.frame(lapply(unstack(na.omit(tdata)), FUN=length))
  k<-(1/(a-1))*(sum(tmp.outj)-(sum(square(tmp.outj))/(sum(tmp.outj))))
  
  var.w<-MSw
  var.a<-(MSa-MSw)/(k)
  r<-var.a/(var.w + var.a)

  low.F<-qf(alpha/2, num.df, denom.df, lower.tail=FALSE)
  N<-dim(na.omit(tdata))[1]
  n.bar<-N/a
  n.not<-n.bar-sum(square(tmp.outj-n.bar)/((a-1)*N))	
    type<-match.arg(CI.type)
      if(type=="THD"){
	up.F<-qf(alpha/2, denom.df, num.df, lower.tail=FALSE)	
	FL<-(MSa/MSw)/low.F
	FU<-(MSa/MSw)*up.F
	low.CI<-(FL-1)/(FL+n.not-1)
	up.CI<-(FU-1)/(FU+n.not-1)
       }
      if(type=="Smith"){
	z.not<-qnorm(alpha/2)
	Vr<-(2*square(1-r)/square(n.not))*((square((1+r*(n.not-1)))/(N-a))+((a-1)*(1-r)*(1+r*(2*n.not-1))+square(r)*(sum(square(tmp.outj))-2*(1/N)*sum((tmp.outj^3))+(1/square(N))*square(sum(square(tmp.outj)))))/square(a-1))
	low.CI<-r+z.not*sqrt(Vr)
	up.CI<-r-z.not*sqrt(Vr) 
      }

list(ICC=r, LowerCI=low.CI, UpperCI=up.CI, N=a, k=k, varw=var.w, vara=var.a)
}
