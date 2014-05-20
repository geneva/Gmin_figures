load("/Users/ageneva/Dropbox/code_snippets/git/gmin_figures/gmin.RData")

plot.gmin <- function(x,cutoff,nm){
  x <- x[which(x$nsites>cutoff),]
  gmin.z <- scale(x$gmin)
  gmin.p <- pnorm(gmin.z)
  x<- cbind(x,gmin.p)
  par(mar=c(3,5,2,2))
  plot(data=x, gmin~start, ylim=c(0,1), type="n", pch=20, las=1, xlab="", ylab="", xaxt="n",axes=F)
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "skyblue")
  box(lwd=2)
  mtext(nm, side=2, cex=1.7, line=3)
  axis(2, las=1, cex.axis=1.5)
  axis(1, at=c(0,5,10,15,20), labels=c(0,5,10,15,20), cex.axis=1.5)
  points(data=x, gmin[which(x$nsites>cutoff)]~start[which(x$nsites>cutoff)], pch=20)
  lines(data=x, gmin[which(x$nsites>cutoff)]~start[which(x$nsites>cutoff)])
  points(data=x[which(x$gmin.p<0.01),],gmin~start,pch=21, col="black")
  points(data=x[which(x$gmin.p<0.01),],gmin~start,pch=20, col="white")
}

read.gmin <- function(file){
  name <- read.table(file)
  gmin <- name[,14]/name[,12]
  name <- cbind(name,gmin)
  keep <- c(1,2,3,4,23)
  name <- name[,keep]
  name[,2:3] <- name[,2:3]/1000000 
  colnames(name) <- c("chr", "start","end", "nsites", "gmin")
  return(name)
}



layout(matrix(c(1,2,3,4,5),5,1,byrow=TRUE))
plot.gmin(chr2L,5000, "2L")
plot.gmin(chr2R,5000, "2R")
plot.gmin(chr3L,5000, "3L")
plot.gmin(chr3R,5000, "3R")
plot.gmin(chrX,5000, "X")





chr2L_50 <- read.gmin("/Volumes/ageneva/maupolyII/Gmin/2L_2sim_Gmin.txt")
chr2R_50 <- read.gmin("/Volumes/ageneva/maupolyII/Gmin/2R_2sim_Gmin.txt")
chr3L_50 <- read.gmin("/Volumes/ageneva/maupolyII/Gmin/3L_2sim_Gmin.txt")
chr3R_50 <- read.gmin("/Volumes/ageneva/maupolyII/Gmin/3R_2sim_Gmin.txt")
chrX_50 <- read.gmin("/Volumes/ageneva/maupolyII/Gmin/X_2sim_Gmin.txt")

layout(matrix(c(1,2,3,4,5),5,1,byrow=TRUE))
plot.gmin(chr2L_50,5000,"chr2L")
plot.gmin(chr2R_50,5000,"chr2R")
plot.gmin(chr3L_50,5000,"chr3L")
plot.gmin(chr3R_50,5000,"chr3R")
plot.gmin(chrX_50,5000,"chrX")


chr2L <- read.gmin("/Volumes/ageneva/maupolyII/Gmin/2L_2sim_Gmin_10kb.txt")
chr2R <- read.gmin("/Volumes/ageneva/maupolyII/Gmin/2R_2sim_Gmin_10kb.txt")
chr3L <- read.gmin("/Volumes/ageneva/maupolyII/Gmin/3L_2sim_Gmin_10kb.txt")
chr3R <- read.gmin("/Volumes/ageneva/maupolyII/Gmin/3R_2sim_Gmin_10kb.txt")
chrX <- read.gmin("/Volumes/ageneva/maupolyII/Gmin/X_2sim_Gmin_10kb.txt")

layout(matrix(c(1,2,3,4,5),5,1,byrow=TRUE))
plot.gmin(chr2L,5000,"chr2L")
plot.gmin(chr2R,5000,"chr2R")
plot.gmin(chr3L,5000,"chr3L")
plot.gmin(chr3R,5000,"chr3R")
plot.gmin(chrX,5000,"chrX")



Gmin_10kb <- rbind(chr2L,chr2R,chr3L,chr3R, chrX)

gmin.z <- scale(Gmin_10kb$gmin)
gmin.p <- pnorm(gmin.z)
Gmin_10kb <- cbind(Gmin_10kb,gmin.p)
head(Gmin_10kb)

layout(matrix(c(1,2,3,4,5),5,1,byrow=TRUE))
plot.gmin(Gmin_10kb[which(Gmin_10kb$chr=="2L"),], 5000, "2L")
plot.gmin(Gmin_10kb[which(Gmin_10kb$chr=="2R"),], 5000, "2R")
plot.gmin(Gmin_10kb[which(Gmin_10kb$chr=="3L"),], 5000, "3L")
plot.gmin(Gmin_10kb[which(Gmin_10kb$chr=="3R"),], 5000, "3R")
plot.gmin(Gmin_10kb[which(Gmin_10kb$chr=="X"),], 5000, "X")




chr2L_win <- read.gmin("/Users/ageneva/Dropbox/X_1kb.txt")
