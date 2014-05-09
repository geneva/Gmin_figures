load("/Users/ageneva/Dropbox/code_snippets/git/gmin_figures/gmin.RData")

plot.gmin <- function(x,cutoff,nm){
  x <- x[which(x$nsites>cutoff),]
  gmin.z <- scale(x$gmin)
  gmin.p <- pnorm(gmin.z)
  x<- cbind(x,gmin.p)
  par(mar=c(3,5,2,2))
  plot(data=x, gmin~start, ylim=c(0,1), type="n", pch=20, las=1, xlab="", ylab="", xaxt="n",axes=F)
  box(lwd=2)
  #mtext(expression(italic("G")[min]), side=2, line=4, cex=1.7)
  mtext(nm, side=2, cex=1.7, line=3)
  axis(2, las=1, cex.axis=1.5)
  axis(1, at=c(0,5,10,15,20), labels=c(0,5,10,15,20), cex.axis=1.5)
  #abline(h=0.28, lty=3) set this to zscore cutoff
  points(data=x, gmin~start, pch=20)
  lines(data=x, gmin~start)
  points(data=x[which(x$gmin.p<0.05),],gmin~start,pch=21, col="black")
  points(data=x[which(x$gmin.p<0.05),],gmin~start,pch=20, col="white")
}



layout(matrix(c(1,2,3,4,5),5,1,byrow=TRUE))
plot.gmin(chr2L,5000, "2L")
plot.gmin(chr2R,5000, "2R")
plot.gmin(chr3L,5000, "3L")
plot.gmin(chr3R,5000, "3R")
plot.gmin(chrX,5000, "X")

