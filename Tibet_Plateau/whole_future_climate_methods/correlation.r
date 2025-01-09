library(tidyverse)
load("sample_prediction_methods.Rdata")
load("../../../whole_tibet.Rdata")

#meta <- result  %>% select(matches("^WT1_|^WT2_|^WT3_|^WT11_|ID"))
meta <- result  %>% select(matches("^T1_|^T2_|^T3_|^T11_|ID"))
rownames(meta) <- meta$ID
meta <- meta[,-1]
meta2 <- meta

me1 <- c("future_index_im", "future_index_all")
me1 <- me1[1]
cor_result_reduced <- list()
for (k2 in me1) {
  cor_result <- list()
  for (k in names(future_index_methods)) {
    future_index <- future_index_methods[[k]]
    future_index <- future_index[[k2]]$now
    meta <- meta2[rownames(future_index),]
    a1 <- sum(rownames(meta) != rownames(future_index))
    if (a1 > 0){
      print("something was wrong")
    }
    cor1 <- data.frame(matrix(0,nrow = ncol(meta), ncol = ncol(future_index)))
    colnames(cor1) <- colnames(future_index)
    rownames(cor1) <- colnames(meta)
    for (i in 1:ncol(meta)) {
      for (j in 1:ncol(future_index)) {
        data1 <- data.frame(x = meta[,i], y = future_index[j])
        colnames(data1) <- c("x","y")
        data1 <- na.omit(data1)
        if (sd(data1$x) > 0){
          z1 <- cor(data1$x,data1$y)
          cor1[i,j] <- z1
        }else{
          cor1[i,j] <- 0
        }
      }
    }
    cor2 <-  cor1[grepl("mean",rownames(cor1)),]
    cor3 <- cor2[apply(cor2,1,max) > 0.5,]
    cor_result[[k]] <- cor3
    print(k)
    print(max(cor3))
  }
  cor_result_reduced[[k2]] <- cor_result
}

data1 <- future_index_methods$RF$future_index_im$now
n1 <- c("linear_m","linear_wm","linear_NQI", 
        "nolinear_m","nolinear_wm","nolinear_NQI")
n2 <- c("L-M","L-WM","L-NQI","NL-M","NL-WM","NL-NQI")
sum(colnames(data1) != n1)
colnames(data1) <- n2

meta <- meta2[rownames(data1),]
meta <- meta %>% select(T1_3_mean)

data1$fNPV <- meta$T1_3_mean
data1 <- na.omit(data1)

data2 <- data1 %>%  pivot_longer(cols = -c(fNPV), names_to = "variable", values_to = "value") 
data2$variable <- factor(data2$variable, levels = c("L-M","L-WM","L-NQI","NL-M","NL-WM","NL-NQI"))

library(ggpubr)
library(ggrastr)
p1 <- ggplot(data2, aes(x = fNPV, y = value, color = variable)) +  
  geom_point_rast(alpha = 0.5) +  
  labs(x = "fNPV", y = "SHI", color = "") +
  scale_fill_manual(limits = c("L-M","L-WM","L-NQI","NL-M","NL-WM","NL-NQI"),
                    values = c("#F8766D","#B79F00","#00BA38","#00BFC4","#619CFF","#F564E3")) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  geom_smooth(method = 'lm', formula = y~x, se = F, show.legend = FALSE) +
  stat_cor(aes(label = paste(after_stat(r.label), sep = '~`,`~')), method = "pearson",  label.x.npc = 'left', label.y.npc = 'top', size =3,
           show.legend=FALSE) +
  theme_classic()

p1
ggsave(p1, file="SHI.pdf", width=6, height=4)
ggsave(p1, file="SHI.png", width=6, height=4)
whole_cor <- p1
save(whole_cor, file = "picture_whole_cor.Rdata")

#####################T1_2
meta <- meta2[rownames(data1),]
meta <- meta %>% select(T1_2_mean)

data1$fNPV <- meta$T1_2_mean
data1 <- na.omit(data1)

data2 <- data1 %>%  pivot_longer(cols = -c(fNPV), names_to = "variable", values_to = "value") 
data2$variable <- factor(data2$variable, levels = c("L-M","L-WM","L-NQI","NL-M","NL-WM","NL-NQI"))

library(ggpubr)
library(ggrastr)
p1 <- ggplot(data2, aes(x = fNPV, y = value, color = variable)) +  
  geom_point_rast(alpha = 0.5) +  
  labs(x = "fNPV", y = "SHI", color = "") +
  scale_fill_manual(limits = c("L-M","L-WM","L-NQI","NL-M","NL-WM","NL-NQI"),
                    values = c("#F8766D","#B79F00","#00BA38","#00BFC4","#619CFF","#F564E3")) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  geom_smooth(method = 'lm', formula = y~x, se = F, show.legend = FALSE) +
  stat_cor(aes(label = paste(after_stat(r.label), sep = '~`,`~')), method = "pearson",  label.x.npc = 'left', label.y.npc = 'top', size =3,
           show.legend=FALSE) +
  theme_classic()

p1
ggsave(p1, file="SHI_T1_2.pdf", width=6, height=4)
ggsave(p1, file="SHI_T1_2.png", width=6, height=4)

#####################T1_9
meta <- meta2[rownames(data1),]
meta <- meta %>% select(T1_9_mean)

data1$fNPV <- meta$T1_9_mean
data1 <- na.omit(data1)

data2 <- data1 %>%  pivot_longer(cols = -c(fNPV), names_to = "variable", values_to = "value") 
data2$variable <- factor(data2$variable, levels = c("L-M","L-WM","L-NQI","NL-M","NL-WM","NL-NQI"))

library(ggpubr)
library(ggrastr)
data2 <- data2[data2$fNPV < 20000,]
p1 <- ggplot(data2, aes(x = fNPV, y = value, color = variable)) +  
  geom_point_rast(alpha = 0.5) +  
  labs(x = "fNPV", y = "SHI", color = "") +
  scale_fill_manual(limits = c("L-M","L-WM","L-NQI","NL-M","NL-WM","NL-NQI"),
                    values = c("#F8766D","#B79F00","#00BA38","#00BFC4","#619CFF","#F564E3")) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  geom_smooth(method = 'lm', formula = y~x, se = F, show.legend = FALSE) +
  stat_cor(aes(label = paste(after_stat(r.label), sep = '~`,`~')), method = "pearson",  label.x.npc = 'left', label.y.npc = 'top', size =3,
           show.legend=FALSE) +
  theme_classic()

p1
ggsave(p1, file="SHI_T1_9.pdf", width=6, height=4)
ggsave(p1, file="SHI_T1_9.png", width=6, height=4)

#####################T2_1
meta <- meta2[rownames(data1),]
meta <- meta %>% select(T2_1_mean)

data1$fNPV <- meta$T2_1_mean
data1 <- na.omit(data1)

data2 <- data1 %>%  pivot_longer(cols = -c(fNPV), names_to = "variable", values_to = "value") 
data2$variable <- factor(data2$variable, levels = c("L-M","L-WM","L-NQI","NL-M","NL-WM","NL-NQI"))

library(ggpubr)
library(ggrastr)
p1 <- ggplot(data2, aes(x = fNPV, y = value, color = variable)) +  
  geom_point_rast(alpha = 0.5) +  
  labs(x = "fNPV", y = "SHI", color = "") +
  scale_fill_manual(limits = c("L-M","L-WM","L-NQI","NL-M","NL-WM","NL-NQI"),
                    values = c("#F8766D","#B79F00","#00BA38","#00BFC4","#619CFF","#F564E3")) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  geom_smooth(method = 'lm', formula = y~x, se = F, show.legend = FALSE) +
  stat_cor(aes(label = paste(after_stat(r.label), sep = '~`,`~')), method = "pearson",  label.x.npc = 'left', label.y.npc = 'top', size =3,
           show.legend=FALSE) +
  theme_classic()

p1
ggsave(p1, file="SHI_T2_1.pdf", width=6, height=4)
ggsave(p1, file="SHI_T2_1.png", width=6, height=4)
