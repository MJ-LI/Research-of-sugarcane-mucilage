library(vegan)
library(ggplot2)
otu=read.csv("root.csv", sep=",", header=T)
otu.dist <- vegdist(subset(otu, select = -group))
m <- monoMDS(otu.dist)

plot(m$points)
dat <- as.data.frame(m$points)
dat$gr <- otu$group
p <- ggplot(dat, aes(MDS1, MDS2, col = gr, shape = gr)) +
   geom_point() +
   geom_jitter(width = 0.00025, height = 0.00025, size = 3)+
   theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12), legend.title = element_blank())+
   theme_bw() +
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p



otu.dist <- vegdist(subset(otu, select = -group))

otu.ano <- anosim(otu.dist, otu$group, permutation = 999) #同上

otu.ano
plot(otu.ano,col = c("#00AFBB", "#E7B800", "#FC4E07"))
