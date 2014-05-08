load("/Users/ageneva/Dropbox/code_snippets/git/gmin_figures/gmin.RData")

plot.gmin <- function(x,cutoff){
  x <- x[which(x$nsites>cutoff),]
  gmin.z <- scale(x$gmin)
  gmin.p <- pnorm(gmin.z)
  x<- cbind(x,gmin.p)
  par(oma=c(8,4,8,4), mar=c(0.5,6,0.5,1))
  plot(data=x, gmin~start, ylim=c(0,1), type="n", pch=20, las=1, xlab="", ylab="", xaxt="n",axes=F)
  box(lwd=2)
  mtext(expression(italic("G")[min]), side=2, line=4, cex=1.7)
  axis(2, las=1, cex.axis=1.5)
  axis(1, at=c(0,5,10,15,20), labels=c(0,5,10,15,20), cex.axis=1.5)
  rect(2503253,-0.03,3063232,1.03, col="gray88", border=NA)
  rect(4917507,-0.03,6037228,1.03, col="gray88", border=NA)
  rect(7082078,-0.03,8046464,1.03, col="gray88", border=NA)
  rect(9666210,-0.03,11272604,1.03, col="gray88", border=NA)
  rect(13823804,-0.03,15335968,1.03, col="gray88", border=NA)
  rect(15647600,-0.03,22422826,1.03, col="gray88", border=NA)
  #abline(h=0.28, lty=3) set this to zscore cutoff
  points(data=x, gmin~start, pch=20)
  lines(data=x, gmin~start)
  points(data=x[which(x$gmin.p<0.05),],gmin~start,pch=21, col="black")
  points(data=x[which(x$gmin.p<0.05),],gmin~start,pch=20, col="white")
}




plot.gmin(chr2L,1000)
plot.gmin(chr2R,1000)
plot.gmin(chr3L,1000)
plot.gmin(chr3R,1000)
plot.gmin(chrX,1000)
