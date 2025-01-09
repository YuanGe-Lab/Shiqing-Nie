library(tidyverse)
load("../final_model_methods.Rdata")
model <- final_modle_methods$RF
n1 <- c("linear_m","linear_wm","linear_NQI", 
        "nolinear_m","nolinear_wm","nolinear_NQI")
n2 <- c("L-M","L-WM","L-NQI","NL-M","NL-WM","NL-NQI")
a <- sum(names(model) != n1)
if (a == 0){
  names(model) <- n2
}else{
  print("somthing was wrong")
}
temp1 <- list()
for (i1 in names(model)) {
  data1 <- model[[i1]]$reduce_im
  asv1 <- data1$final_variable
  data2 <- data.frame(asv = asv1, class = i1)
  temp1[[i1]] <- data2
}
data_asv <- bind_rows(temp1)

asv_id <- unique(data_asv$asv)
length(unique(data_asv$asv))

library(ggplot2)
ggplot(data = data_asv,aes(x = class, y = asv)) +
  geom_point()

data_asv$value <- data_asv$class
temp1 <- data_asv %>%   
  pivot_wider(names_from = class, values_from = value)   

a <- sort(temp1$asv[!is.na(temp1$`NL-NQI`)])
b <- sort(model$`NL-NQI`$reduce_im$final_variable)
a == b
asv_prediction <- temp1
save(asv_prediction,file = "asv_prediction.Rdata")

tax <- read.delim("/mnt/data/tibet/dada2/taxonomy/classification.txt")
tax <- tax[tax$asv %in% asv_prediction$asv,]
final_asv <- asv_prediction %>% full_join(tax, by = "asv")

load("/mnt/data/tibet/dada2/core_asv/core_asv.Rdata")
seq1 <- read.delim("/mnt/data/tibet/dada2/core_asv/top.fasta",header = F)
seq2 <- data.frame(id = seq1$V1[seq(1,nrow(seq1),2)], seq = seq1$V1[seq(2,nrow(seq1),2)])

seq2$id <- gsub(">","",seq2$id)
sum(!(seq2$id %in% rownames(core_asv$top)))
rownames(seq2) <- seq2$id

final_asv2 <- final_asv %>% left_join(seq2,  by = c("asv" = "id"))

write.table(final_asv2, file = "final_asv.txt", row.names = F,sep = "\t", quote = F)
###########################asv correlation with SHI#####################
load("../../../../core_asv.Rdata")

core_asv <- core_asv$top
prediction_table <- core_asv[rownames(core_asv) %in% asv_prediction$asv,]
save(prediction_table,file = "prediction_table.Rdata")

index <- read.table("/mnt/data/tibet/dada2/core_asv/heathy_index/index.txt",row.names = 1)
final_asv <- data.frame(t(prediction_table))

index <- index[rownames(index) %in% rownames(final_asv),]
final_asv <- final_asv[rownames(index),]

library(Hmisc)
corr_matrix <-  rcorr(as.matrix(index),as.matrix(final_asv), type = 'spearman')
r <- corr_matrix$r
p <- corr_matrix$P
r <- r[-c(1:ncol(index)),1:ncol(index)]
p <- p[-c(1:ncol(index)),1:ncol(index)]
r[p >= 0.05] <- 0

write.table(data.frame(ID = rownames(r),r), file = "final_asv_correlation.txt", row.names = F,sep = "\t", quote = F)

a <- sum(colnames(r) != c("linear_m","linear_wm","linear_NQI","nolinear_m","nolinear_wm","nolinear_NQI"))
if (a > 0){
  print()
}
colnames(r) <- c("L_M","L_WM","L_NQI","NL_M","NL_WM","NL_NQI")
library(ggheatmap)
p <- ggheatmap(r,cluster_rows = T, legendName = "Correlation") %>%  ggheatmap_theme(1,theme =list(theme(axis.text.x = element_text(angle = 0))))
ggsave(p, file="final_asv_correlation.png", width=6, height=6)
