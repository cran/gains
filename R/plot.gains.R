plot.gains<-function(x, col1="darkred", col2="blue4", 
                     xlab="Depth of File",
                     ylab="Sample Mean Response",
                     leg1="Mean Response",
                     leg2="Cumulative Mean Response",
                     type="b",
                     pch1=1,pch2=1,
                     lty1=1,lty2=1,
                     main="Gains Table Plot",...) 
{
   if (x$percents==TRUE) {
     plot(x$depth, x$mean.resp, col=col1, type=type, pch=pch1,
          xlab=xlab,
          ylab=ylab,yaxt="n",
          main=main)
     axt<-axTicks(2)
     axis(side=2,at=axt,labels=paste(axt*100,"%",sep=""))
     points(x$depth, x$cume.mean.resp, col=col2, type=type, pch=pch2)
     legend("topright",legend=c(leg1,leg2),
          col=c(col1,col2),lty=c(lty1,lty2))
   } else
   {
     plot(x$depth, x$mean.resp, col=col1, type=type, pch=pch1,
          xlab=xlab,
          ylab=ylab,
          main=main)
     points(x$depth, x$cume.mean.resp, col=col2, type=type, pch=pch2)
     legend("topright",legend=c(leg1,leg2),
          col=c(col1,col2),lty=c(lty1,lty2))
   }
}
