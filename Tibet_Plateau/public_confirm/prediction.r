load("../final_model_methods.Rdata")
library(tidymodels)
library(rules)
library(baguette)
library(tidyverse)
tidymodels_prefer()

load("/mnt/data/tibet/public/public_health_asv.Rdata")
load("/mnt/data/tibet/dada2/core_asv/heathy_index/asv/core_climate_0.3_add_others/asv_climate.Rdata")
for (i in 1:length(public_result)) {
  asv1 <- public_result[[names(public_result)[i]]]
  for (j in 1:length(asv1)) {
    asv2 <- asv1[[names(asv1)[j]]]
    if (ncol(asv2) == 0 | nrow(asv2) == 0){
      public_result[[names(public_result)[i]]][[names(asv1)[j]]] <- NA
      next
    }
    asv3 <- asv2[,colnames(asv2) %in% asv_climate]
    if (ncol(asv3) == 0){
      public_result[[names(public_result)[i]]][[names(asv1)[j]]] <- NA
      next
    }
    asv_lack <- asv_climate[!(asv_climate %in% colnames(asv3))]
    if (length(asv_lack) > 0){
      asv_temp <- asv3[,1:length(asv_lack)]
      asv_temp <- data.frame(asv_temp) %>%
        mutate(across(everything(), ~0))
      colnames(asv_temp) <- asv_lack
      asv4 <- cbind(asv3,asv_temp)
      if (length(asv4) != length(asv_climate)){
        print("asv4 length is not right")
        break
      }
    }else{
      asv4 <- as.data.frame(asv3)
    }
    asv4 <- asv4[,asv_climate]
    public_result[[names(public_result)[i]]][[names(asv1)[j]]] <- asv4
  }
}



public_index <- list()

final_modle <- final_modle_methods$RF

for (k in 1:length(public_result)) {
  public_asv <- public_result[[names(public_result)[k]]]
  future_index_im <- list()
  future_index_all <- list()
  for (i1 in 1:length(public_asv)) {
    if (length(public_asv[[names(public_asv)[i1]]]) < 2){
      future_index_im[[names(public_asv)[i1]]] <- NA
      future_index_all[[names(public_asv)[i1]]] <- NA
      next
    }
    meta  <- public_asv[[i1]]
    data1 <- data.frame(matrix(0,nrow = nrow(meta),ncol = length(final_modle)))
    rownames(data1) <- rownames(meta)
    colnames(data1) <- names(final_modle)
    colnames(data1) == names(final_modle)
    future_index_im[[names(public_asv)[i1]]] <-  data1
    future_index_all[[names(public_asv)[i1]]] <-  data1
    meta <- round(meta)
    for (j1 in names(final_modle)) {
      model1_im <- final_modle[[j1]]$reduce_im$model
      meta_im <- meta[,colnames(meta) %in% final_modle[[j1]]$reduce_im$final_variable]
      predict1 <- predict(model1_im,meta_im)
      future_index_im[[names(public_asv)[i1]]][,j1] <- predict1
      
      model1_all <- final_modle[[j1]]$reduce_all$model
      meta_all <- meta[,colnames(meta) %in% final_modle[[j1]]$reduce_all$final_variable]
      predict2 <- predict(model1_all,meta_all)
      future_index_all[[names(public_asv)[i1]]][,j1] <- predict2
    }
  }
  public_index[[names(public_result)[k]]] <- list(future_index_im = future_index_im,
                                                  future_index_all = future_index_all)
}
save(public_index,file = "public_index.Rdata")

