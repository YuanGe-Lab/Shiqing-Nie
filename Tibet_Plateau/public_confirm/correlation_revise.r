library(tidyverse)
load("public_index.Rdata")
meta <- read.delim("/mnt/data/tibet/public/tibet_public.txt")

meta <- meta  %>% select(matches("^T1_|^T2_|^T3_|^T11_|ID")) %>% select(matches("mean|ID"))

rownames(meta) <- meta$ID
meta <- meta[,-1]
meta2 <- meta

me1 <- c("future_index_im", "future_index_all")
cor_result_reduced <- list()
cor_number_reduced <- list()
for (k in me1) {
  #l1 <- names(public_index)[1] 
  #l2 <- names(df1)[1]
  cor_result <- list()
  cor_number <- list()
  for (l1 in names(public_index)) {
    cor_result[[l1]] <- list()
    cor_number[[l1]] <- list()
    df1 <- public_index[[l1]][[k]]
    for (l2 in names(df1)) {
      index <- df1[[l2]]
      meta <- meta2[rownames(index),]
      if (nrow(meta) < 2){
        cor_result[[l1]][[l2]] <- NA
        cor_number[[l1]][[l2]] <- NA
        next
      }
      a <- sum(rownames(meta) != rownames(index))
      if (a > 0){
        print("something was wronmg")
        break
      }
      cor1 <- data.frame(matrix(0,nrow = ncol(meta), ncol = ncol(index)))
      colnames(cor1) <- colnames(index)
      rownames(cor1) <- colnames(meta)
      number1 <- cor1
      for (i in 1:ncol(meta)) {
        for (j in 1:ncol(index)) {
          data1 <- data.frame(x = meta[,i], y = index[j])
          colnames(data1) <- c("x","y")
          data1 <- na.omit(data1)
          number1[i,j] <- nrow(data1)
          if (nrow(data1) < 2 ){
            cor1[i,j] <- 0
          }else if (sd(data1$x) > 0){
            z1 <- cor(data1$x,data1$y)
            cor1[i,j] <- z1
          }else{
            cor1[i,j] <- 0
          }
        }
      }
      cor2 <-  cor1[grepl("mean",rownames(cor1)),]
      cor3 <- cor2
      cor_result[[l1]][[l2]] <- cor3
      cor_number[[l1]][[l2]] <- number1
    }
  }
  cor_result_reduced[[k]] <- cor_result
  cor_number_reduced[[k]] <- cor_number
}
#save(cor_result_reduced,file = "cor_result.Rdata")
#save(cor_number_reduced,file = "cor_number.Rdata")

#load("cor_result.Rdata")
#load("cor_number.Rdata")
############choice best results###############
# final_r <- cor_result_reduced
# for (i in me1) {
#   data1 <- final_r[[i]]
#   me2 <- names(data1)
#   for (j in me2) {
#     data2 <- data1[[j]]
#     me3 <- names(data2)
#     for (k in me3) {
#       data3 <- data2[[k]]
#       if(is.null(data3)){
#         final_r[[i]][[j]][[k]] <- NULL
#       }
#       if("T1_3_mean" %in% rownames(data3)){
#         data4 <- data3["T1_3_mean",]
#         n1 <- cor_number_reduced[[i]][[j]][[k]]["T1_3_mean",]
#         n2 <- unique(as.numeric(n1))
#         if (length(n2) > 1){
#           print(i)
#           print(j)
#           print(k)
#           print("something was wrong n2")
#         }
#         data4$number <- n2
#         rownames(data4) <- k
#         final_r[[i]][[j]][[k]] <- data4
#       }else{
#         final_r[[i]][[j]][[k]] <- NULL
#       }
#     }
#     final_r[[i]][[j]] <- bind_rows(final_r[[i]][[j]])
#   }
# }
final_r <- cor_result_reduced$future_index_im$public_whole_dada2
final_r[is.na(final_r)] <- NULL
id1 <- rownames(final_r$PRJNA731395)

result1 <- list()
for (k in id1) {
  for (j in names(final_r)) {
    data2 <- final_r[[j]]
    if(sum(rownames(data2) != id1) > 0){
      print("something was wrong")
    }
    data3 <- data2[k,]
    rownames(data3) <- j
    result1[[k]][[j]] <- data3
  }
  result1[[k]] <- bind_rows(result1[[k]])
}
id2 <- c("PRJNA1019819","PRJNA626532","PRJNA722150", "PRJNA731395")
for (i in names(result1)) {
  result1[[i]] <- result1[[i]][id2,]
}


best_cor <- result1$T1_3_mean
best_cor <- best_cor[id2,]
best_cor <- best_cor[!grepl("_",rownames(best_cor)),]
best_cor$id <- rownames(best_cor)
n1 <- c("linear_m","linear_wm","linear_NQI", 
        "nolinear_m","nolinear_wm","nolinear_NQI")
n2 <- c("L-M","L-WM","L-NQI","NL-M","NL-WM","NL-NQI")
a <- sum(colnames(best_cor)[1:6] != n1)
if (a > 0){
  print("sonmthing was wrong a")
}
colnames(best_cor)[1:6] <- n2
data1 <- best_cor %>%  pivot_longer(cols = -c(id), names_to = "variable", values_to = "value")  
mean(data1$value)
#0.6957958
data1$variable <- factor(data1$variable, levels = c("L-M","L-WM","L-NQI","NL-M","NL-WM","NL-NQI"))


p1 <- ggplot(data1,aes(x = variable, y = value, color = id)) + 
  geom_point() +
  labs(x = "", y = "Correlation", color = "Bioproject") +  
  theme_classic() +
  theme(axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0))
ggsave(p1, file="correlation.pdf", width=6, height=4)
ggsave(p1, file="correlation.png", width=6, height=4)
public <- p1
save(public, file = "picture_public.Rdata")



best_cor <- result1$T1_2_mean
best_cor <- best_cor[-3,]

best_cor$id <- rownames(best_cor)
n1 <- c("linear_m","linear_wm","linear_NQI", 
        "nolinear_m","nolinear_wm","nolinear_NQI")
n2 <- c("L-M","L-WM","L-NQI","NL-M","NL-WM","NL-NQI")
a <- sum(colnames(best_cor)[1:6] != n1)
if (a > 0){
  print("sonmthing was wrong a")
}
colnames(best_cor)[1:6] <- n2
data1 <- best_cor %>%  pivot_longer(cols = -c(id), names_to = "variable", values_to = "value")  
mean(data1$value)
#0.6957958
data1$variable <- factor(data1$variable, levels = c("L-M","L-WM","L-NQI","NL-M","NL-WM","NL-NQI"))


p2 <- ggplot(data1,aes(x = variable, y = value, color = id)) + 
  geom_point() +
  labs(x = "", y = "Correlation", color = "Bioproject") +  
  theme_classic() +
  theme(axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0))
ggsave(p2, file="correlation_T1_2.pdf", width=6, height=4)
ggsave(p2, file="correlation_T1_2.png", width=6, height=4)

best_cor <- result1$T1_9_mean

best_cor <- best_cor[-3,]

best_cor$id <- rownames(best_cor)
n1 <- c("linear_m","linear_wm","linear_NQI", 
        "nolinear_m","nolinear_wm","nolinear_NQI")
n2 <- c("L-M","L-WM","L-NQI","NL-M","NL-WM","NL-NQI")
a <- sum(colnames(best_cor)[1:6] != n1)
if (a > 0){
  print("sonmthing was wrong a")
}
colnames(best_cor)[1:6] <- n2
data1 <- best_cor %>%  pivot_longer(cols = -c(id), names_to = "variable", values_to = "value")  
mean(data1$value)
#0.6957958
data1$variable <- factor(data1$variable, levels = c("L-M","L-WM","L-NQI","NL-M","NL-WM","NL-NQI"))


p3 <- ggplot(data1,aes(x = variable, y = value, color = id)) + 
  geom_point() +
  labs(x = "", y = "Correlation", color = "Bioproject") +  
  theme_classic() +
  theme(axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0))
ggsave(p2, file="correlation_T1_9.pdf", width=6, height=4)
ggsave(p2, file="correlation_T1_9.png", width=6, height=4)


best_cor <- result1$T2_1_mean

#best_cor <- best_cor[-3,]

best_cor$id <- rownames(best_cor)
n1 <- c("linear_m","linear_wm","linear_NQI", 
        "nolinear_m","nolinear_wm","nolinear_NQI")
n2 <- c("L-M","L-WM","L-NQI","NL-M","NL-WM","NL-NQI")
a <- sum(colnames(best_cor)[1:6] != n1)
if (a > 0){
  print("sonmthing was wrong a")
}
colnames(best_cor)[1:6] <- n2
data1 <- best_cor %>%  pivot_longer(cols = -c(id), names_to = "variable", values_to = "value")  
mean(data1$value)
#0.6957958
data1$variable <- factor(data1$variable, levels = c("L-M","L-WM","L-NQI","NL-M","NL-WM","NL-NQI"))


p4 <- ggplot(data1,aes(x = variable, y = value, color = id)) + 
  geom_point() +
  labs(x = "", y = "Correlation", color = "Bioproject") +  
  theme_classic() +
  theme(axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0))
ggsave(p4, file="correlation_T2_1.pdf", width=6, height=4)
ggsave(p4, file="correlation_T2_1.png", width=6, height=4)
