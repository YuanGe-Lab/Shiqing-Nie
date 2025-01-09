load("race.Rdata")
library(tidymodels)
library(rules)
library(baguette)
tidymodels_prefer()
library(DALEXtra)
library(forcats)
library(stringr)

library(doParallel)
# Create a cluster object and then register: 
cl <- makePSOCKcluster(50)
registerDoParallel(cl)
#########choice the best model#######################
index_id <- names(race_data)


best <- c("RF")



final_modle_methods <- list()
for (i2 in best) {
  final_modle <- list()
  best_id <- i2
  print(i2)
  for (i1 in 1:length(index_id)) {
    final_modle[[index_id[i1]]] <- list()
    data1 <- race_data[[index_id[i1]]]
    
    final_modle[[index_id[i1]]]$mehods <- best_id
    best_results <- 
      data1$best_parameters[[best_id]]
    
    best_model <- 
      data1$workflow[[best_id]] %>% 
      finalize_workflow(best_results) %>% 
      fit(data1$data)
    
    vip_features <- colnames(data1$data %>% select(-outcome1))
    vip_train <-
      data1$data %>%
      select(all_of(vip_features))
    explainer_final <-
      explain_tidymodels(
        best_model,
        data = vip_train,
        y = data1$data$outcome1,
        label = "final",
        verbose = F
      )
    set.seed(1803)
    vip_final <- model_parts(explainer_final, loss_function = loss_root_mean_square)
    #pic1 <- ggplot_imp(vip_final)
    
    im1 <- vip_final %>% 
      filter(!str_detect(variable, "^_")) %>% 
      select(-c(label,permutation)) %>%
      group_by(variable) %>% 
      summarise(im = mean(dropout_loss)) %>%
      arrange(desc(im))
    ########origin performance#########
    
    keep_pred <- control_resamples(save_pred = F, save_workflow = F, parallel_over = "everything")
    set.seed(1)
    data_folds <- vfold_cv(data1$data, strata = outcome1, repeats = 5)
    
    origin_ten <- 
      best_model %>% 
      fit_resamples(resamples = data_folds, control = keep_pred)
    z <- collect_metrics(origin_ten) #   average result
    origin_r_square <- z$mean[2]
    origin_r_square_initial <- origin_r_square
    #collect_metrics(origin_ten,summarize = F)  # each fold
    
    ######reduce variable based on importance#######
    max1 <- nrow(im1)-1
    max_variable <- im1$variable
    final_variable <- im1$variable
    final_model_temp <- best_model
    print("reduce variable based on importance")
    print(index_id[i1])
    for (j in seq(max1,2,-1)) {
      variable1 <- im1$variable[1:j]
      model1 <- best_model
      formula1 <- formula(paste0("outcome1 ~", paste0(variable1,collapse = " + ")))
      removed_model <- model1 %>%  remove_variables() %>%  add_formula(formula1)
      # removed_model <- model1 %>% update_variables(outcomes = compressive_strength, predictors = all_of(variable1))  #不知道怎么回事，update_variables 对于fit_resamples会报错。all_of(variable1)找不到
      removed_ten <- removed_model %>% 
        fit_resamples(resamples = data_folds, control = keep_pred)
      z <- collect_metrics(removed_ten) #   average result
      removed_r_square <- z$mean[2]
      print(length(variable1))
      print(removed_r_square)
      if (removed_r_square > origin_r_square){
        origin_r_square <- removed_r_square
        final_variable <- variable1
        max_variable <- variable1
        final_model_temp <- removed_model
      }else if (removed_r_square > (origin_r_square * 0.99)){
        final_variable <- variable1
        final_model_temp <- removed_model
      }else{
        break
      }
    }
    
    final_reduced_model_im <- list()
    final_reduced_model_im[["best_parameters"]] <- best_results
    final_model_temp <- final_model_temp %>% fit(data1$data)
    final_reduced_model_im[["model"]] <- final_model_temp
    final_reduced_model_im[["final_variable"]] <- final_variable
    final_reduced_model_im[["max_variable"]] <- max_variable
    
    ###########reduce variable based on im_all compare (save time vs all)#########################
    variable1 <- im1$variable
    variable1 <- rev(variable1)
    variable2 <- variable1
    origin_r_square <- origin_r_square_initial
    max1 <- nrow(im1)-1
    max_variable <- variable2
    final_variable <- variable2
    final_model_temp <- best_model
    
    print("reduce variable based on all compare")
    #print(index_id[i1])
    for (i in 1:max1) {
      print(paste0("remove ",i))
      diff0 <- -100
      flag <- "n"
      for (j in 1:length(variable2)) {
        variable1 <- variable2[-j]
        model1 <- best_model
        formula1 <- formula(paste0("outcome1 ~", paste0(variable1,collapse = " + ")))
        removed_model <- model1 %>%  remove_variables() %>%  add_formula(formula1)
        removed_ten <- removed_model %>% 
          fit_resamples(resamples = data_folds, control = keep_pred)
        z <- collect_metrics(removed_ten) #   average result
        removed_r_square <- z$mean[2]
        # print(length(variable1))
        # print(removed_r_square)
        print(removed_r_square)
        diff <- removed_r_square - origin_r_square 
        if (removed_r_square > origin_r_square){
          origin_r_square <- removed_r_square
          max_variable <- variable1
          if (diff >  diff0){
            remove_id <- j
            diff0 <- diff
            final_model_temp <- removed_model
            final_variable <- variable1
          }
          flag <- "y"
          break
        }else if (removed_r_square > (origin_r_square * 0.99)){
          if (diff >  diff0){
            remove_id <- j
            diff0 <- diff
            final_model_temp <- removed_model
            final_variable <- variable1
          }
          flag <- "y"
          break
        }else{
        }
      }
      if (flag == "n"){
        break
      }
      variable2 <- variable2[-remove_id]
    }
    
    final_reduced_model_all <- list()
    final_reduced_model_all[["best_parameters"]] <- best_results
    final_model_temp <- final_model_temp %>% fit(data1$data)
    final_reduced_model_all[["model"]] <- final_model_temp
    final_reduced_model_all[["final_variable"]] <- final_variable
    final_reduced_model_all[["max_variable"]] <- max_variable
    
    final_modle[[index_id[i1]]]$mehods <- best_id
    final_modle[[index_id[i1]]]$reduce_im <- final_reduced_model_im
    final_modle[[index_id[i1]]]$reduce_all <- final_reduced_model_all
    final_modle[[index_id[i1]]]$origin_r_square_initial <- origin_r_square_initial
    
  }
  final_modle_methods[[i2]] <- final_modle
}

save(final_modle_methods,file = "final_model_methods.Rdata")
stopCluster(cl) # end paralle


final_modle <- final_modle_methods$RF
#####################last performance################################
performance_last <- data.frame(matrix(0,nrow = length(final_modle) * 4, ncol = 4))
colnames(performance_last) <- c("R-square","type1","type2","type3")
performance_last$type1 <- rep(c("Train","Train","Test","Test"),length(final_modle))
performance_last$type2 <- rep(c("im","all"),length(final_modle) * 2)
performance_last$type3 <- rep(names(final_modle),each = 4)
n <- 0
train_im <- list()
train_all <- list()
test_im <- list()
test_all <- list()
for (i in names(final_modle)) {
  print(i)
  n <- n + 1
  data_train <- race_data[[i]]$data_train
  data_test <- race_data[[i]]$data_test
  model_im <- final_modle[[i]]$reduce_im$model
  model_all <- final_modle[[i]]$reduce_all$model
  model_im_fit <- model_im %>% fit(data_train)
  model_all_fit <- model_all %>% fit(data_train)
  
  train_rsq <- data.frame(truth = data_train$outcome1, .pred = predict(model_im_fit,data_train))
  train_im[[i]] <- train_rsq
  train_im[[i]]$id <- i
  train_rsq_v <- rsq(train_rsq,truth,.pred)
  train_rsq <- train_rsq_v$.estimate
  performance_last$`R-square`[n] <- train_rsq
  n <- n + 1
  train_rsq <- data.frame(truth = data_train$outcome1, .pred = predict(model_all_fit,data_train))
  train_all[[i]] <- train_rsq
  train_all[[i]]$id <- i
  train_rsq_v <- rsq(train_rsq,truth,.pred)
  train_rsq <- train_rsq_v$.estimate
  performance_last$`R-square`[n] <- train_rsq
  n <- n + 1
  test_rsq <- data.frame(truth = data_test$outcome1, .pred = predict(model_im_fit,data_test))
  test_im[[i]] <- test_rsq
  test_im[[i]]$id <- i
  test_rsq_v <- rsq(test_rsq,truth,.pred)
  test_rsq <- test_rsq_v$.estimate
  performance_last$`R-square`[n] <- test_rsq
  n <- n + 1
  test_rsq <- data.frame(truth = data_test$outcome1, .pred = predict(model_all_fit,data_test))
  test_all[[i]] <- test_rsq
  test_all[[i]]$id <- i
  test_rsq_v <- rsq(test_rsq,truth,.pred)
  test_rsq <- test_rsq_v$.estimate
  performance_last$`R-square`[n] <- test_rsq
}
train_im <- bind_rows(train_im)
train_all <- bind_rows(train_all)
test_im <- bind_rows(test_im)
test_all <- bind_rows(test_all)
performance_last <- list(train_im = train_im, train_all = train_all,
                         test_im = test_im,test_all = test_all,
                         R2 = performance_last)
# data1 <- final_modle$linear_m$performance
# data1 <- data1[data1$im > 0,]
# ggplot(data1, aes(x=number,y=im)) + geom_point()
save(performance_last,file = "performance_last.Rdata")
#load("performance_last.Rdata")
# data1 <- performance_last$R2
# colnames(data1)[1] <- "R2"
# ggplot(data1,aes(x = type1, y = R2, color = type3)) + geom_point()

library(ggpmisc)
n1 <- names(performance_last)[1:4]
n1 <- n1[c(1,3)]
dir.create("performance")
performance1 <- list()

library(ggrastr)
for (i in n1) {
  data <- performance_last[[i]]
  n1 <- c("linear_m","linear_wm","linear_NQI", 
          "nolinear_m","nolinear_wm","nolinear_NQI")
  n2 <- c("L-M","L-WM","L-NQI","NL-M","NL-WM","NL-NQI")
  for (j in 1:nrow(data)) {
    data$id[j] <- n2[data$id[j] == n1]
  }
  data$id <- factor(data$id, levels = c("L-M","L-WM","L-NQI","NL-M","NL-WM","NL-NQI"))
  p1 <- ggplot(data,aes(x = truth, y = .pred, color = id)) + geom_point_rast(alpha = 0.5) +
    labs(x = "Truth", y = "Prediction", color = "SHI") +
    scale_fill_manual(limits = c("L-M","L-WM","L-NQI","NL-M","NL-WM","NL-NQI"),
                      values = c("#F8766D","#B79F00","#00BA38","#00BFC4","#619CFF","#F564E3")) +
    guides(color = guide_legend(override.aes = list(alpha = 1))) +
    geom_smooth(method = 'lm', formula = y~x, se = F, show.legend = FALSE) +
    stat_poly_eq(aes(label = paste(after_stat(rr.label), sep = '~`,`~')),
                 formula = y~x, parse = TRUE, label.x.npc = 'right', label.y.npc = 'bottom', size = 3) +
    theme_classic()
  #ggsave(p1, file=paste0("performance/",i,".pdf"), width=6, height=4)
  ggsave(p1, file=paste0("performance/",i,".png"), width=6, height=4)
  performance1[[i]] <- p1
}
save(performance1,file = "picture_performance.Rdata")




