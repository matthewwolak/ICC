effort<-function(est.type=c("hypothetical", "pilot"), e=NULL, ICC=NULL, x=NULL, y=NULL, data=NULL, alpha=0.05){
  
  effort.call <- Call <-match.call()

  is.wholenumber<-function(x, tol = .Machine$double.eps^0.5) {
       abs(x - round(x)) < tol
       }

  CIW<-function(r, n, k){
       num.df<-n-1
       denom.df<-n*(k-1)
       F.stat<-(1-r+r*k)/(1-r)
       low.F<-qf(alpha/2, num.df, denom.df, lower.tail=FALSE)
       up.F<-qf(alpha/2, denom.df, num.df, lower.tail=FALSE)
       FL<-F.stat/low.F
       FU<-F.stat*up.F
       LowCI<-(FL-1)/(FL+k-1)
       UpCI<-(FU-1)/(FU+k-1)
     list(LowCI=LowCI, UpCI=UpCI, w=UpCI-LowCI)
     }

  k.maker<-function(effort, out=c("pairs", "widths")){
       pot.ens<-seq(3, effort/2, by=1)
       pot.ks<-effort/pot.ens
       ens<-sort(pot.ens, decreasing=FALSE)
       ks<-sort(pot.ks, decreasing=TRUE)
       typeb<-match.arg(out)
      if(typeb=="pairs"){ 
       pairs<-cbind(ks, ens)
       }
      else{
       ciw<-CIW(ICC, n=ens, k=ks)$w
       }
     }

  type<-match.arg(est.type)
    if(type=="hypothetical"){
      pairs<-sapply(e, k.maker, "p")
      widths<-sapply(e, k.maker, "w")
      }
    
    if(type=="pilot"){
      square<-function(z){z^2}
      xc<-as.character(effort.call$x)
      yc<-as.character(effort.call$y)
      inds<-unique(data[xc])[[1]]
      a<-length(inds)
      tdata<-data.frame(data[yc], data[xc])
      if(!is.factor(tdata[,2])){
        tdata[,2]<-as.factor(tdata[,2])
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
      ICC<-var.a/(var.w + var.a)
      EN<-dim(tdata)[1]
      e<-c(round(EN-EN*.15, 0), EN, round(EN+EN*.15, 0)) 
      pairs<-sapply(e, k.maker, "p")
      widths<-sapply(e, k.maker, "w")
      }

results<-list(e=e, pairs=pairs, widths=widths, ICC=ICC)

   line.types<-rep(c("dashed", "solid", "dotted"), length.out=length(e))
 plot(results$widths[[1]]~results$pairs[[1]][,1], 
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
