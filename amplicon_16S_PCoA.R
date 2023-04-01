
#if (FALSE){
#  source("https://bioconductor.org/biocLite.R")
#  biocLite(c("ggplot2","vegan"))
#}
library("ggplot2")
library("vegan")

design = read.table("map.txt", header=T, row.names= 1, sep="\t") 

Bray_curtis = read.csv("Bray.csv", sep=",", header=T, row.names = 1, check.names=F)

idx = rownames(design) %in% colnames(Bray_curtis) 
sub_design = design[idx,]
Bray_curtis = Bray_curtis[rownames(sub_design), rownames(sub_design)]

pcoa = cmdscale(Bray_curtis, k=3, eig=T) 

write.table(pcoa$points,"points.txt", sep=",")


points = as.data.frame(pcoa$points)
colnames(points) = c("x", "y", "z") 
eig = pcoa$eig
points = cbind(points, sub_design[match(rownames(points), rownames(design)), ])




p = ggplot(points, aes(x=x, y=y, color=treatment)) +
  geom_point(alpha=.7, size=2) + 
  stat_ellipse(level = 0.8) +
  labs(x=paste("PCoA 1 (", format(100 * eig[1] / sum(eig), digits=4), "%)", sep=""),
       y=paste("PCoA 2 (", format(100 * eig[2] / sum(eig), digits=4), "%)", sep=""),
       title="Bray_curtis PCoA") +
       geom_jitter(width = 0.000035, height = 0.000035, size = 2) +
       theme_bw() +
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())  
p