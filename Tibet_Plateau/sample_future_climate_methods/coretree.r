library(tidyverse)
load("asv_prediction.Rdata")
library(treeio)
library(ape)
tree_origin <- read.newick("/mnt/data/tibet/dada2/seperate/rooted-tree-all/tree.nwk")
tree <- keep.tip(tree_origin, asv_prediction$asv)
library(ggtree)
tr <- ggtree(tree)

load("/mnt/data/tibet/dada2/taxonomy/seqtax.Rdata")
tax <- tax[asv_prediction$asv,]
tax[is.na(tax)] <- "Unclassified"
length(unique(tax$Phylum))
table(tax$Phylum)

#taxtree$Phylum <- gsub(" ", "", taxtree$Phylum)#全部替换
taxtree <- tax
taxid <- unique(taxtree$Phylum)
grp <- list()
for (i in taxid){
  grp[[i]] <- rownames(taxtree)[taxtree$Phylum == i]
}
tr <- groupOTU(tr, grp, 'Phylum') + aes(color=Phylum) + #geom_tiplab() + 
  #geom_text2(aes(subset =!isTip, label = node)) +
#  theme(legend.justification = c(0,0),
#        legend.text = element_text(face="italic")) +###图列斜体
scale_color_manual(limits = taxid,
                   values = c("#FF0033","#CCEBC5","#FB8072","#80B1D3",
                              "#B3DE69","#FCCDE5","#9966FF","#BC80BD","#FFED6F"),
                   name = "Phylum ", 
                   guide=guide_legend(
                     override.aes=list(linewidth = 3))) #自动
tr
#library(ggplot2)
asv_prediction <- data.frame(asv_prediction)
rownames(asv_prediction) <- asv_prediction$asv
asv_prediction <- asv_prediction[,-1]
df <- asv_prediction

colnames(df) <- c("L-M","L-WM","L-NQI","NL-M","NL-WM","NL-NQI")
f <- gheatmap(tr, df, offset=0, width=3, font.size=3, colnames = T,
              colnames_angle=-50, hjust=0, legend_title = "SHI") + 
  vexpand(0.1,direction = -1) + #guides(colour = guide_legend(size = 1)) +
  scale_fill_manual(limits = c("L-M","L-WM","L-NQI","NL-M","NL-WM","NL-NQI"),
                    values = c("#F8766D","#B79F00","#00BA38","#00BFC4","#619CFF","#F564E3"),
                    na.value = "gray", name = "SHI", guide="none")
  
f

#ggsave(f, file="prediction_tree.pdf", width=6, height=6)
ggsave(f, file="prediction_tree.png", width=6, height=6)
tree <- f
save(tree,file="picture_tree.Rdata")
