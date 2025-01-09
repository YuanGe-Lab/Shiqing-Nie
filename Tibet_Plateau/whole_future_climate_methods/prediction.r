load("../final_model_methods.Rdata")
library(tidymodels)
library(rules)
library(baguette)
tidymodels_prefer()

load("../../../asv/core_climate_0.3_add_others/whole_prediction/sample_prediction.Rdata")

future_index_methods <- list()
for (i2 in "RF") {
  print(i2)
  future_index_im <- list()
  future_index_all <- list()
  final_modle <- final_modle_methods[[i2]]
  for (i1 in 1:length(future_asv)) {
    #print(i1)
    meta  <- future_asv[[i1]]
    data1 <- data.frame(matrix(0,nrow = nrow(meta),ncol = length(final_modle)))
    rownames(data1) <- rownames(meta)
    colnames(data1) <- names(final_modle)
    colnames(data1) == names(final_modle)
    future_index_im[[names(future_asv)[i1]]] <-  data1
    future_index_all[[names(future_asv)[i1]]] <-  data1
    meta <- round(meta)
    for (j1 in names(final_modle)) {
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
  future_index_methods[[i2]] <- list(future_index_im = future_index_im,
                                     future_index_all = future_index_all)
}
save(future_index_methods,file = "sample_prediction_methods.Rdata")
load("sample_prediction_methods.Rdata")

up  <- function(x){
  sum(x > 0) / length(x)
}
down  <- function(x){
  sum(x < 0) / length(x)
}

m2 <- c("future_index_im")
picture_propotion <- list()
for (k1 in m2) {
  performance <- list()
  z1 <- future_index_methods$RF[[k1]]
  for (i2 in names(future_index_methods)) {
    performance1 <- data.frame(matrix(0,ncol = ncol(z1$bio1_1.5) + 4,nrow = length(z1) * 3))
    colnames(performance1) <- c(colnames(z1$bio1_1.5),"mean","climate","methods","type")
    future_index <- future_index_methods[[i2]][[k1]]
    performance1$methods <- i2
    performance1$climate <- rep(names(future_index),3)
    performance1$type <- rep(c("mean","propotion","down"),each = length(z1))
    now <- future_index$now
    for (i in 2:length(future_index)) {
      id <- names(future_index)[i]
      future1 <- future_index[[i]]
      a <- sum(rownames(future1) != rownames(now))
      b <- sum(colnames(future1) != colnames(now))
      sum(rownames(future1) != rownames(now))
      if (a == 0){
        if (b == 0){
          print("everything is ok")
        }
      }
      data1 <- future1 - now
      performance1[i,1:6] <- apply(data1[,1:6], 2, mean)
      performance1[i,7] <- sum(data1) / (ncol(data1) * nrow(data1))
      performance1[length(z1) + i,1:6] <- apply(data1[,1:6], 2, up)
      performance1[length(z1) + i,7] <- sum(data1 > 0) / (ncol(data1) * nrow(data1))
      performance1[(length(z1) * 2) + i,1:6] <- apply(data1[,1:6], 2, down)
      performance1[(length(z1) * 2) + i,7] <- sum(data1 < 0) / (ncol(data1) * nrow(data1))
      z2 <- sum(data1 ==0) 
      if ((z2 / nrow(data1)) > 0.1){
        print(z2)
      }
    }
    performance[[i2]] <- performance1
  }
  
  performance_last <- do.call(rbind,performance) %>% filter(climate != "now")
  id1 <- strsplit(k1,split = "_")[[1]]
  id1 <- id1[length(id1)]
  save(performance_last,file = paste0(id1,"_performance.Rdata"))
  
  library(tidyverse)
  data1 <- performance_last %>% select(-mean) %>%
    pivot_longer(cols = -c(climate,methods,type), names_to = "variable", values_to = "value")  
  n1 <- c("linear_m","linear_wm","linear_NQI", 
          "nolinear_m","nolinear_wm","nolinear_NQI")
  n2 <- c("L-M","L-WM","L-NQI","NL-M","NL-WM","NL-NQI")
  
  for (j in 1:nrow(data1)) {
    data1$variable[j] <- n2[data1$variable[j] == n1]
  }
  
  data1$variable <- factor(data1$variable, levels = c("L-M","L-WM","L-NQI","NL-M","NL-WM","NL-NQI"))
  data1$climate <-gsub("bio1","Climate",data1$climate)
  data1$climate <-gsub("^c","C",data1$climate)
  
  data1$climate <- factor(data1$climate, levels = c("Climate_1.5","Climate_ssp126_40","Climate_ssp585_40","Climate_ssp126_100","Climate_ssp585_100"))
  
  p_mean <- data1 %>% filter(type == "mean") %>%
    ggplot(aes(x = climate, y = value,color = variable, group = variable)) +
    geom_point() +
    geom_line(linetype = 2) + 
    scale_color_manual(limits = c("L-M","L-WM","L-NQI","NL-M","NL-WM","NL-NQI"),
                      values = c("#F8766D","#B79F00","#00BA38","#00BFC4","#619CFF","#F564E3")) +
    scale_x_discrete(labels = c("simple_1.5","ssp126_40","ssp585_40","ssp126_100","ssp585_100")) +
    labs(x = "Future climate", y= "Mean", color = "SHI") +
    #facet_wrap(~methods, scales = "fixed") +
    #theme(axis.text.x =  element_text(angle = 30,hjust = 1,vjust = 1))
    theme_classic() +
    theme(axis.text.x = element_text(angle = 30,hjust = 1,vjust = 1))
  p_mean
  ggsave(p_mean, file=paste0(id1,"_mean_performance.png"),width = 6,height = 4)
  
  p_up <- data1 %>% filter(type == "propotion") %>%
    ggplot(aes(x = climate, y = value,color = variable, group = variable)) +
    geom_point() +
    geom_line(linetype = 2) + 
    geom_hline(yintercept = 0.5, linetype = "dotted", color = "red") +
    scale_color_manual(limits = c("L-M","L-WM","L-NQI","NL-M","NL-WM","NL-NQI"),
                       values = c("#F8766D","#B79F00","#00BA38","#00BFC4","#619CFF","#F564E3")) +
    scale_x_discrete(labels = c("simple_1.5","ssp126_40","ssp585_40","ssp126_100","ssp585_100")) +
    labs(x = "Future climate", y= "Increased propotion", color = "SHI") +
    #facet_wrap(~methods, scales = "fixed") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 30,hjust = 1,vjust = 1))
  p_up
  ggsave(p_up, file=paste0(id1,"_up_performance.png"),width = 6,height = 4)
  
  p_down <- data1 %>% filter(type == "down") %>%
    ggplot(aes(x = climate, y = value,color = variable, group = variable)) +
    geom_point() +
    geom_line(linetype = 2) + 
    geom_hline(yintercept = 0.5, linetype = "dotted", color = "red") +
    scale_color_manual(limits = c("L-M","L-WM","L-NQI","NL-M","NL-WM","NL-NQI"),
                       values = c("#F8766D","#B79F00","#00BA38","#00BFC4","#619CFF","#F564E3")) +
    scale_x_discrete(labels = c("simple_1.5","ssp126_40","ssp585_40","ssp126_100","ssp585_100")) +
    labs(x = "Future climate", y= "Decreased propotion", color = "SHI") +
    #facet_wrap(~methods, scales = "fixed") +
    theme_classic() + 
    theme(axis.text.x = element_text(angle = 30,hjust = 1,vjust = 1))
  p_down
  picture_propotion[[k1]]$mean <- p_mean 
  picture_propotion[[k1]]$up <- p_up
  picture_propotion[[k1]]$down <- p_down
  ggsave(p_down, file=paste0(id1,"_down_performance.png"),width = 6,height = 4)
}
p_up <- picture_propotion$future_index_im$up
p_down <- picture_propotion$future_index_im$down
save(p_down, file = "picture_down.Rdata")

down_data <- data1 %>% filter(type == "down")
save(down_data,file = "down_data.Rdata")

load("im_performance.Rdata")
write.table(performance_last,"im_performance.xls",col.names=NA, sep = "\t", quote = F) 
