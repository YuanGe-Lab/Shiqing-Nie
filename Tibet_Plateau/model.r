dir.create("race")
race_data <- list()

load("asv.Rdata")
load("index.Rdata")
for (iloop in 1:ncol(index)) {
  data1 <- asv
  data1$outcome1 <- index[,iloop]
  data1 <- na.omit(data1)
  
  library(tidymodels)
  library(rules)
  library(baguette)
  tidymodels_prefer()
  # data(concrete, package = "modeldata")
  # glimpse(concrete)
  # data1 <- 
  #   concrete %>% 
  #   group_by(across(-compressive_strength)) %>% 
  #   summarize(compressive_strength = mean(compressive_strength),
  #             .groups = "drop")
  # nrow(data1)
  # 
  set.seed(1)
  # #default 3:1 ratio
  # 
  # data1 <- data1 %>% rename(outcome1 = compressive_strength)
  data1_split <- initial_split(data1, strata = outcome1, prop = 0.9)
  data1_train <- training(data1_split)
  data1_test  <- testing(data1_split)
  
  set.seed(1502)
  data1_folds <- 
    vfold_cv(data1_train, strata = outcome1, repeats = 5)
  
  # normalized_rec <- 
  #   recipe(outcome1 ~ ., data = data1_train) %>% 
  #   step_normalize(all_predictors()) 
  # poly_recipe <- 
  #   normalized_rec %>% 
  #   step_poly(all_predictors()) %>% 
  #   step_interact(~ all_predictors():all_predictors())
  
  ##############define model#############################
  linearsimple_reg_spec <-
    linear_reg() %>%
    set_engine("lm") #%>% translate()
  
  linear_reg_spec <- 
    linear_reg(penalty = tune(), mixture = tune()) %>% 
    set_engine("glmnet")
  
  nnet_spec <- 
    mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>% 
    set_engine("nnet", MaxNWts = 2600) %>% 
    set_mode("regression")
  
  mars_spec <- 
    mars(prod_degree = tune()) %>%  #<- use GCV to choose terms
    set_engine("earth") %>% 
    set_mode("regression")
  
  
  svm_r_spec <- 
    svm_rbf(cost = tune(), rbf_sigma = tune()) %>% 
    set_engine("kernlab") %>% 
    set_mode("regression")
  
  svm_p_spec <- 
    svm_poly(cost = tune(), degree = tune()) %>% 
    set_engine("kernlab") %>% 
    set_mode("regression")
  
  svm_l_spec <-
    svm_linear(cost = tune(), margin = tune()) %>%
    set_engine('kernlab') %>%
    set_mode('regression')
  
  knn_spec <- 
    nearest_neighbor(neighbors = tune(), dist_power = tune(), weight_func = tune()) %>% 
    set_engine("kknn") %>% 
    set_mode("regression")
  
  cart_spec <- 
    decision_tree(cost_complexity = tune(), min_n = tune()) %>% 
    set_engine("rpart") %>% 
    set_mode("regression")
  
  
  bag_cart_spec <- 
    bag_tree() %>% 
    set_engine("rpart", times = 50L) %>% 
    set_mode("regression")
  
  rf_spec <- 
    rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
    set_engine("ranger") %>% 
    set_mode("regression")
  
  xgb_spec <- 
    boost_tree(tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(), 
               min_n = tune(), sample_size = tune(), trees = tune()) %>% 
    set_engine("xgboost") %>% 
    set_mode("regression")
  
  cubist_spec <- 
    cubist_rules(committees = tune(), neighbors = tune()) %>% 
    set_engine("Cubist") 
  #The analysis in M. Kuhn and Johnson (2013) specifies that the neural network should have up to 27 hidden units in the layer. The extract_parameter_set_dials() function extracts the parameter set, which we modify to have the correct parameter range:
  nnet_param <- 
    nnet_spec %>% 
    extract_parameter_set_dials() %>% 
    update(hidden_units = hidden_units(c(1, 27)))
  
  ###################combine recipe and models##############################
  
  # normalized <- 
  #   workflow_set(
  #     preproc = list(normalized = normalized_rec), 
  #     models = list(SVM_radial = svm_r_spec, SVM_poly = svm_p_spec, SVM_linear = svm_l_spec,
  #                   KNN = knn_spec, neural_network = nnet_spec)
  #   )
  # normalized
  # normalized %>% extract_workflow(id = "normalized_KNN")
  
  #The option column is a placeholder for any arguments to use when we evaluate the workflow. For example, to add the neural network parameter object:

  
  model_vars <- 
    workflow_variables(outcomes = outcome1, 
                       predictors = everything())
  
  no_pre_proc <- 
    workflow_set(
      preproc = list(simple = model_vars), 
      models = list(MARS = mars_spec, CART = cart_spec, CART_bagged = bag_cart_spec,
                    RF = rf_spec, boosting = xgb_spec, Cubist = cubist_spec,linear_reg = linear_reg_spec, 
                    linear_lm_reg = linearsimple_reg_spec,SVM_radial = svm_r_spec, 
                    SVM_poly = svm_p_spec, SVM_linear = svm_l_spec,
                    KNN = knn_spec, neural_network = nnet_spec)
    )
  no_pre_proc
  
  no_pre_proc <- 
    no_pre_proc %>% 
    option_add(param_info = nnet_param, id = "simple_neural_network")
  no_pre_proc
  #for this soil health  data, it dose not work
  # with_features <- 
  #   workflow_set(
  #     preproc = list(full_quad = poly_recipe), 
  #     models = list(linear_reg = linear_reg_spec, linear_lm_reg = linearsimple_reg_spec, KNN = knn_spec)
  #   )
  
  
  # all_workflows <- 
  #   bind_rows(no_pre_proc, normalized, with_features) %>% 
  #   # Make the workflow ID's a little more simple: 
  #   mutate(wflow_id = gsub("(simple_)|(normalized_)", "", wflow_id))
  all_workflows <-
    #bind_rows(no_pre_proc, normalized) %>%
    bind_rows(no_pre_proc) %>%
    # Make the workflow ID's a little more simple:
    mutate(wflow_id = gsub("(simple_)|(normalized_)", "", wflow_id))
  all_workflows
  
  # grid_ctrl <-
  #   control_grid(
  #     save_pred = TRUE,
  #     parallel_over = "everything",
  #     save_workflow = TRUE
  #   )
  
  # All operating systems
  library(doParallel)
  
  # Create a cluster object and then register: 
  cl <- makePSOCKcluster(60)
  registerDoParallel(cl)
  
  # Now run fit_resamples()`...
  
  # time.start <- Sys.time()
  # 
  # grid_results <-
  #   all_workflows %>%
  #   workflow_map(
  #     seed = 1503,
  #     resamples = data1_folds,
  #     grid = 25,
  #     control = grid_ctrl
  #   )
  # time.end <- Sys.time()
  # time.running <- time.end - time.start
  # print(time.running)
  # 
  # grid_results
  # grid_results %>% 
  #   rank_results() %>% 
  #   filter(.metric == "rmse") %>% 
  #   select(model, .config, rmse = mean, rank)
  # 
  # # autoplot(
  # #   grid_results,
  # #   rank_metric = "rmse",  # <- how to order models
  # #   metric = "rmse",       # <- which metric to visualize
  # #   select_best = TRUE     # <- one point per workflow
  # # ) +
  # #   geom_text(aes(y = mean - 1/2, label = wflow_id), angle = 90, hjust = 1) +
  # #   #lims(y = c(3.5, 9.5)) + 
  # #   theme(legend.position = "none")
  # 
  # autoplot(
  #   grid_results,
  #   rank_metric = "rsq",  # <- how to order models
  #   metric = "rsq",       # <- which metric to visualize
  #   select_best = TRUE     # <- one point per workflow
  # ) +
  #   geom_text(aes(y = mean - 0.02, label = wflow_id), angle = 90, hjust = 1) +
  #   lims(y = c(0.65, 0.95)) + 
  #   theme(legend.position = "none")
  # 
  # autoplot(grid_results, id = "Cubist", metric = "rmse")
  
  ######15.4 EFFICIENTLY SCREENING MODELS######
  library(finetune)
  
  time.start <- Sys.time()
  
  race_ctrl <-
    control_race(
      save_pred = TRUE,
      parallel_over = "everything",
      save_workflow = TRUE
    )
  
  race_results <-
    all_workflows %>%
    workflow_map(
      "tune_race_anova",
      seed = 1503,
      resamples = data1_folds,
      grid = 30,
      control = race_ctrl
    )
  race_results
  time.end <- Sys.time()
  time.running <- time.end - time.start
  print(time.running)
  
  stopCluster(cl) # end paralle
  p1 <- autoplot(
    race_results,
    rank_metric = "rsq",  # <- how to order models
    metric = "rsq",       # <- which metric to visualize
    select_best = TRUE     # <- one point per workflow
  ) +
    geom_text(aes(y = mean - 0.02, label = wflow_id), angle = 90, hjust = 1) +
    lims(y = c(0.65, 0.95)) + 
    theme(legend.position = "none")
  ggsave(p1,file=paste0("race/",colnames(index)[iloop],"_race.png"),width = 5,height = 4)
  #autoplot(race_results, id = "Cubist", metric = "rmse")
  z <- p1$data
  p1 <- ggplot(z,aes(x = rank, y = mean,color = wflow_id)) +
    geom_point() +
    geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err),width = 0.1) +
    geom_text(aes(y = mean - 0.02, label = wflow_id), angle = 90, hjust = 1) +
    lims(y = c(0.65, 0.95)) + 
    theme(legend.position = "none")
  
  race_data[[colnames(index)[iloop]]] <- list()
  
  methods_id <- race_results$wflow_id
  race_data[[colnames(index)[iloop]]]$data <-  data1
  race_data[[colnames(index)[iloop]]]$data_split <-  data1_split
  race_data[[colnames(index)[iloop]]]$data_train <-  data1_train
  race_data[[colnames(index)[iloop]]]$data_test <-  data1_test
  
  race_data[[colnames(index)[iloop]]]$picture <- p1
  race_data[[colnames(index)[iloop]]]$parameters <- list()
  race_data[[colnames(index)[iloop]]]$best_parameters <- list()
  race_data[[colnames(index)[iloop]]]$workflow <- list()
  #race_data[[colnames(index)[iloop]]]$best_last_fit <- list()
  for (me1  in 1:length(methods_id)) {
    print(paste0("now runing ", methods_id[me1]))
    temp1 <- race_results[me1,]$result[[1]] %>%
      select(id2,id,.metrics)
    temp2 <- temp1  %>% unnest(cols = c(.metrics))
    temp3 <- temp2 %>% filter(.metric == "rsq") %>% select(-c(id2,id,.metric,.estimator,.config))
    race_data[[colnames(index)[iloop]]]$parameters[[methods_id[me1]]] <- temp3
    # temp4 <- temp3  %>%  pivot_longer(cols = -.estimate) %>% 
    #   group_by(name,value) %>% summarise(mean = mean(.estimate),.groups = "drop")
    # p1 <- ggplot(temp4,aes(x = value, y = mean)) +
    #   geom_point() +
    #   geom_line() +
    #   labs(x = "", y = "rsq") +
    #   facet_wrap(~name, scales = "free")
    # p1
    # ggsave(p1,file=paste0("grid/",colnames(index)[iloop],"_",methods_id[me1],"_","_grid.png"),width = 5,height = 4)
    best_results <- 
      race_results %>% 
      extract_workflow_set_result(methods_id[me1]) %>% 
      select_best(metric = "rsq")
    race_data[[colnames(index)[iloop]]]$best_parameters[[methods_id[me1]]] <- best_results
    
    workflow_ex <- race_results %>% 
      extract_workflow(methods_id[me1])
    race_data[[colnames(index)[iloop]]]$workflow[[methods_id[me1]]] <- workflow_ex
    
    # best_test_results <- 
    #   race_results %>% 
    #   extract_workflow(methods_id[me1]) %>% 
    #   finalize_workflow(best_results) %>% 
    #   last_fit(split = data1_split)
    # race_data[[colnames(index)[iloop]]]$best_last_fit[[methods_id[me1]]] <- best_test_results
  }
}
save(race_data,file = "race.Rdata")


