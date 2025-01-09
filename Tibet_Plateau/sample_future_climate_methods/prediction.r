load("../final_model_methods.Rdata")
library(tidymodels)
library(rules)
library(baguette)
tidymodels_prefer()

load("../../../asv/core_climate_0.3_add_others/sample_prediction/sample_prediction.Rdata")


index <- read.delim("../../../index.txt",row.names = 1)
index2 <- index

future_index_methods <- list()

for (i2 in "RF") {
  print(i2)
  future_index_im <- list()
  future_index_all <- list()
  final_modle <- final_modle_methods[[i2]]
  for (i1 in 1:length(future_asv)) {
    meta  <- future_asv[[i1]]
    meta <- meta[rownames(meta) %in% rownames(index),]
    index <- index2[rownames(meta),]
    sum(rownames(meta) != rownames(index))
    colnames(index) == names(final_modle)
    future_index_im[[names(future_asv)[i1]]] <-  index
    future_index_all[[names(future_asv)[i1]]] <-  index
    meta <- round(meta)
    for (j1 in colnames(index)) {
      model1_im <- final_modle[[j1]]$reduce_im$model
      meta_im <- meta[,colnames(meta) %in% final_modle[[j1]]$reduce_im$final_variable]
      predict1 <- predict(model1_im,meta_im)
      future_index_im[[names(future_asv)[i1]]][,j1] <- predict1
      
      model1_all <- final_modle[[j1]]$reduce_all$model
      meta_all <- meta[,colnames(meta) %in% final_modle[[j1]]$reduce_all$final_variable]
      predict2 <- predict(model1_all,meta_all)
      future_index_all[[names(future_asv)[i1]]][,j1] <- predict2
    }
  }
  #cor(future_asv$climate_ssp585_100$asv7,asv$asv7)
  future_index_methods[[i2]] <- list(future_index_im = future_index_im,
                                     future_index_all = future_index_all)
}
save(future_index_methods,file = "sample_prediction_methods.Rdata")

pro  <- function(x){
  sum(x > 0) / length(x)
}

m2 <- c("future_index_im", "future_index_all")
for (k1 in m2) {
  z1 <- future_index_methods$RF[[k1]]
  index <- read.delim("../../../index.txt",row.names = 1)
  index2 <- index
  index <- index[rownames(z1$bio1_1.5),]
  
  performance <- list()
  for (i2 in names(future_index_methods)) {
    performance1 <- data.frame(matrix(0,ncol = ncol(z1$bio1_1.5) + 4,nrow = length(z1) * 2))
    colnames(performance1) <- c(colnames(z1$bio1_1.5),"mean","climate","methods","type")
    future_index <- future_index_methods[[i2]][[k1]]
    performance1$methods <- i2
    performance1$climate <- rep(names(future_index),2)
    performance1$type <- rep(c("mean","propotion"),each = length(z1))
    for (i in 1:length(future_index)) {
      id <- names(future_index)[i]
      future1 <- future_index[[i]]
      a <- sum(rownames(future1) != rownames(index))
      b <- sum(colnames(future1) != colnames(index))
      if (a == 0){
        if (b == 0){
          print("everything is ok")
        }
      }
      data1 <- future1 - index
      
      performance1[i,1:6] <- apply(data1[,1:6], 2, mean)
      performance1[i,7] <- sum(data1) / (ncol(data1) * nrow(data1))
      performance1[length(z1) + i,1:6] <- apply(data1[,1:6], 2, pro)
      performance1[length(z1) + i,7] <- sum(data1 > 0) / (ncol(data1) * nrow(data1))
      z2 <- sum(data1 ==0) 
      if (z2 > 0){
        print(z2)
      }
      # data2 <- data1 %>% pivot_longer(cols = -c(id,x), names_to = "variable", values_to = "value")  
      # data2$class <- 1
      # data2$class[data2$value > 0] <- "up"
      # data2$class[data2$value <= 0] <- "down"
      # p <- ggplot(data2, aes(x = x, y = value,color = class)) +
      #   geom_point() +
      #   facet_wrap(~variable, scales = "free")
      # ggsave(p, file=paste0(id,".png"))  
    }
    performance[[i2]] <- performance1
  }
  performance_last <- do.call(rbind,performance)
  id1 <- strsplit(k1,split = "_")[[1]]
  id1 <- id1[length(id1)]
  save(performance_last,file = paste0(id1,"_performance.Rdata"))
  
  library(tidyverse)
  data1 <- performance_last %>%
    pivot_longer(cols = -c(climate,methods,type), names_to = "variable", values_to = "value")  
  p_mean <- data1 %>% filter(type == "mean") %>%
    ggplot(aes(x = variable, y = value,color = climate)) +
    geom_point() +
    facet_wrap(~methods, scales = "fixed") +
    theme(axis.text.x =  element_text(angle = 30,hjust = 1,vjust = 1))
  p_mean
  ggsave(p_mean, file=paste0(paste0(id1,"_mean_performance.png")),width = 9,height = 6)
  
  p_pro <- data1 %>% filter(type == "propotion") %>%
    ggplot(aes(x = variable, y = value,color = climate)) +
    geom_point() +
    #facet_wrap(~methods, scales = "fixed") +
    theme(axis.text.x =  element_text(angle = 30,hjust = 1,vjust = 1))
  p_pro
  ggsave(p_pro, file=paste0(id1,"_pro_performance.png"),width = 9,height = 6)
}
