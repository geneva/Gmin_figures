library(ape)


trees <- read.tree("/Volumes/ageneva/maupolyII/Gmin/3L_portion_1kb.trees")

for (i in 1:length(trees)){
trees[[i]] <- root(multi2di(trees[[i]]), "mel", resolve.root=TRUE)
}



for (l in 1:length(trees)){
plot(ladderize(trees[[l]]), main=paste("Tree ",l,sep=""))
}


chr3L_win <- read.gmin("/Volumes/ageneva/maupolyII/Gmin/3L_portion_1kb.txt")
chr2R_win <- read.gmin("/Volumes/ageneva/maupolyII/Gmin/2R_portion_1kb.txt")

plot(gmin~start, data=chr2R_win, ylim=c(0,1))
