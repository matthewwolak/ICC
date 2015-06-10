ICCbare<-function(x, y, data){
  ICCcall <- Call <-match.call()
  xc<-as.character(ICCcall[[2L]])
  yc<-as.character(ICCcall[[3L]])
  inds<-unique(data[xc])[[1]]
  tdata<-data.frame(data[yc], data[xc])
  if(!is.factor(tdata[,2])){
    warning("x has been coerced to a factor")
    tdata[,2]<-as.factor(tdata[,2])
    }  

  fmla<-formula(tdata)
  if (!is.list(replications(fmla, tdata))){
   tmp1<-aggregate(tdata[,1], list(tdata[,2]),FUN=mean)
   tmp2<-aggregate(tdata[,1], list(tdata[,2]), FUN=length)
   ord.data<-tdata[order(tdata[,2]),]
   Treat.m<-rep(tmp1$x,tmp2$x)
   Among<-(Treat.m-rep(mean(tdata[,1]),nrow(tdata)))^2
   Within<-(ord.data[,1]-Treat.m)^2
   MS<-(c(sum(Among),sum(Within)))/(c(length(tmp2$x)-1, length(tmp2$x)*(tmp2$x[1]-1)))
   var.a<-(MS[1]-MS[2])/tmp2$x[1]
   r<-var.a/(var.a+MS[2])
      }
  else{
     tmpbb<-anova(aov(fmla, data=tdata))
     MSa<-tmpbb[3][1,1]
     tmp.outj<-data.frame(lapply(unstack(na.omit(tdata)), FUN=length))
     var.a<-(MSa-tmpbb[3][2,1])/((1/(length(inds)-1))*(sum(tmp.outj)-(sum((tmp.outj^2))/(sum(tmp.outj)))))
     r<-var.a/(tmpbb[3][2,1] + var.a) 
      }
  list(ICC=r)
}

