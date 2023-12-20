#control host
library(data.table)
li <- c("bray_curtis_distance_matrix", 
        "jaccard_distance_matrix",
        "unweighted_unifrac_distance_matrix",
        "weighted_unifrac_distance_matrix")
#with ML bray 0.25  weight 0.04 
#with ge bray 0.1  weight 0.08
#with cl2 bray 0.21  weight 0.13
#with clw bray 0.22  weight 0.15   
i <- 4
bray <- fread(paste0("../../core-metrics-results/",li[i],"/distance-matrix.tsv"),
              header = T)
bray <- as.data.frame(bray)
rownames(bray) <- bray$V1
bray <- bray[,-1]
brayd <- as.dist(bray)
#meta <- read.delim("../new_15928samples/meta.txt")
#meta <- meta[,c(1,16,30,31)]
#meta <- meta[meta$order != "Primates",]
di <- c("ML_wd.txt","NJ_wd.txt")
j <- 1
phydis <- fread(paste0("../../new_15928samples/tree/",di[j]),
                header = T)
phydis <- as.data.frame(phydis)
rownames(phydis) <- colnames(phydis)
sum(colnames(phydis) == colnames(bray))
phydisd <- as.dist(phydis)


k <- 1
gl <- c("site_dis.txt","dis_2.txt","dis_whole.txt")
geo <- fread(paste0("../../map_geodistance/distance2_whole/",gl[k]),
             header = T)
geo <- as.data.frame(geo)
rownames(geo) <- geo$V1
geo <- geo[,-1]
sum(rownames(geo) == rownames(bray))
geod <- as.dist(geo)

k <- 3
gl <- c("site_dis.txt","dis_2.txt","dis_whole.txt")
geocl <- fread(paste0("../../map_geodistance/distance2_whole/",gl[k]),
               header = T)
geocl <- as.data.frame(geocl)
rownames(geocl) <- geocl$V1
geocl <- geocl[,-1]
sum(rownames(geocl) == rownames(bray))
geocld <- as.dist(geocl)
seq1 <- seq(min(phydisd),max(phydisd),(max(phydisd)/400))
data1 <-  data.frame(matrix(0,nrow=length(seq1),ncol = 7))
colnames(data1) <- c("distance","pphy_r","pphy_p","pgeo_r","pgeo_p",
                     "pgeocl_r","pgeocl_p")
data1$distance <- seq1
for (i in 1:length(seq1)) {
  choice <- which(phydisd <= seq1[i])
  if (length(choice) > 20000){
    seq2 <- seq(1,length(choice),floor(length(choice)/20000))
    choice <- choice[seq2]
  }
  
  if (max(phydisd[choice]) == 0){
    pphy_p <- 0
    pphy_r <- 0
  }else{
    pphy <- cor.test(brayd[choice],phydisd[choice],method = "spearman")
    pphy_p <- pphy$p.value
    pphy_r <- pphy$estimate
  }
  data1[i,2] <- pphy_r
  data1[i,3] <- pphy_p
  
  if (max(geod[choice]) == 0){
    pgeo_p <- 0
    pgeo_r <- 0
  }else{
    pgeo <- cor.test(brayd[choice ],geod[choice],method = "spearman")
    pgeo_p <- pgeo$p.value
    pgeo_r <- pgeo$estimate
  }
  data1[i,4] <- pgeo_r
  data1[i,5] <- pgeo_p
  
  if (max(geocld[choice][!is.na(geocld[choice])]) == 0){
    pgeocl_p <- 0
    pgeocl_r <- 0
  }else{
    pgeocl <- cor.test(brayd[choice ],geocld[choice],method = "spearman")
    pgeocl_p <- pgeocl$p.value
    pgeocl_r <- pgeocl$estimate
  }
  data1[i,6] <- pgeocl_r
  data1[i,7] <- pgeocl_p
}
a <- sum(data1$pphy_p > 0.01)
b <- sum(data1$pgeo_p > 0.01)
c <- sum(data1$pgeocl_p > 0.01)
w <- a + b + c
print(w)

library(ggplot2)
library(ggpubr)#correlation  add p3  相关
data1 <- data1[,c("distance","pphy_r","pgeo_r",
                  "pgeocl_r")]
colnames(data1) <- c("distance","Phylogenetic distance",
                     "Geographical distance","Climatic distance")
library(reshape2)
data1 <- melt(data1,id = "distance")
max1 <- aggregate(.~variable, data=data1[,2:3] ,max)
max1$distance <- 0
for (i in 1:nrow(max1)) {
  max1[i,3] <- data1$distance[data1$value == max1$value[i]]
}

scaleFUN <- function(x){
  sprintf("%.2f", x)
}
p1 <- ggplot(data1,aes(distance,value,color = variable))+
  geom_point(size = 1, alpha = 0.5) +
  scale_color_manual(values = c("Phylogenetic distance" = "#F8766D",
                                "Climatic distance" = "#619CFF",
                                "Geographical distance" = "#00BA38")) +
  #  facet_wrap(~variable, scales = "free") +#y轴自由 
  geom_text(data = max1, aes(x = distance, y = value, color = variable,
                             label = paste0(round(distance,3),"-",round(value,3))), 
            vjust = 1, hjust = 0,show.legend=FALSE) +
  labs( x = "Phylogenetic distance", y = "Spearman correlation",
        color = "") #+
#  scale_y_continuous(label = scaleFUN) +
#  geom_smooth(method = 'lm', formula = y~x, se = TRUE, show.legend = FALSE) +
#  stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = '~`,`~')), method = 'pearson',
#           label.x.npc = 'left', label.y.npc = 'top', size = 5)
p1 
ggsave(p1, file="host_step_new.pdf", width=6, height=2)
p2 <- p1 +   xlim(c(0,1.7))
p2
ggsave(p2, file="host_step_new2.pdf", width=5, height=2)
rm(bray,geo,geocl,phydis,brayd,geod,geocld,phydisd)
save.image("step_new.Rdata")#保存所有
