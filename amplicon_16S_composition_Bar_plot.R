library(reshape2)
library(ggplot2)
zi <- read.delim('family.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)

zi$Taxonomy <- factor(rownames(zi), levels = rev(rownames(zi)))
zi <- melt(zi, id = 'Taxonomy')

p <- ggplot(zi, aes(variable, 100 * value, fill = Taxonomy)) +
  geom_col(position = 'stack', width = 0.6) +
  labs(x = '', y = 'Relative Abundance(%)') +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p <- p +
  scale_fill_manual(values =  rev(c('#6666CC', '#FF9900','#CC3300','#22CC66', '#66CC99','#CC6666'))) +
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent')) +
  theme(legend.title = element_blank())
  
p
