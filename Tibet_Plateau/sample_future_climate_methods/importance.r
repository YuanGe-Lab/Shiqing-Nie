library(tidymodels)
library(rules)
library(baguette)
tidymodels_prefer()
library(DALEXtra)
library(forcats)
library(stringr)

library(forcats)
ggplot_imp <- function(...) {
  obj <- list(...)
  metric_name <- attr(obj[[1]], "loss_name")
  metric_lab <- paste(metric_name, 
                      "after permutations\n(higher indicates more important)")
  
  full_vip <- bind_rows(obj) %>%
    filter(variable != "_baseline_")
  
  perm_vals <- full_vip %>% 
    filter(variable == "_full_model_") %>% 
    group_by(label) %>% 
    summarise(dropout_loss = mean(dropout_loss))
  
  p <- full_vip %>%
    filter(variable != "_full_model_") %>% 
    mutate(variable = fct_reorder(variable, dropout_loss)) %>%
    ggplot(aes(dropout_loss, variable)) 
  if(length(obj) > 1) {
    p <- p + 
      facet_wrap(vars(label)) +
      geom_vline(data = perm_vals, aes(xintercept = dropout_loss, color = label),
                 linewidth = 1.4, lty = 2, alpha = 0.7) +
      geom_boxplot(aes(color = label, fill = label), alpha = 0.2)
  } else {
    p <- p + 
      geom_vline(data = perm_vals, aes(xintercept = dropout_loss),
                 linewidth = 1.4, lty = 2, alpha = 0.7) +
      geom_boxplot(fill = "#91CBD765", alpha = 0.4)
    
  }
  p +
    theme(legend.position = "none") +
    labs(x = metric_lab, 
         y = NULL,  fill = NULL,  color = NULL)
}
# Create a cluster object and then register: 


load("../final_model_methods.Rdata")
final_model <- final_modle_methods$RF

importance_result <- list()

n1 <- c("linear_m","linear_wm","linear_NQI", 
        "nolinear_m","nolinear_wm","nolinear_NQI")
n2 <- c("L-M","L-WM","L-NQI","NL-M","NL-WM","NL-NQI")
for (i in names(final_model)) {
  data1 <- final_model[[i]]
  data1 <- data1$reduce_im
  model1 <- data1$model
  vip_features <- data1$final_variable
  vip_train <-
    model1$pre$mold$predictors
  explainer_final <-
    explain_tidymodels(
      model1,
      data = vip_train,
      y = model1$pre$mold$outcomes,
      label = "final",
      verbose = F
    )
  set.seed(1803)
  vip_final <- model_parts(explainer_final, loss_function = loss_root_mean_square)
  ggplot_imp(vip_final)
  

  im1 <- vip_final %>% 
    filter(!str_detect(variable, "^_")) %>% 
    select(-c(label,permutation)) #%>%
    #group_by(variable) %>% 
    #summarise(im = mean(dropout_loss)) %>%
    #arrange(desc(im))
  n3 <- n2[n1 == i]
  p1 <- ggplot(im1,aes(x = reorder(variable,dropout_loss,decreasing = F),y = dropout_loss)) + geom_boxplot() + labs(x = "", y = n3) + theme_classic() + coord_flip()
  importance_result[[i]][["picture"]] <- p1
  im1 <- im1 %>%
    group_by(variable) %>%
    summarise(im = mean(dropout_loss)) %>%
    arrange(desc(im))
  importance_result[[i]][["data"]] <- im1
}
library(patchwork)
p1 <- importance_result$linear_m$picture + 
importance_result$linear_wm$picture + 
importance_result$linear_NQI$picture + 
importance_result$nolinear_m$picture + 
importance_result$nolinear_wm$picture + 
importance_result$nolinear_NQI$picture
ggsave(p1, file = "importance.pdf",height = 10, width = 8)
ggsave(p1, file = "importance.png",height = 10, width = 8)

save(importance_result, file = "importance.Rdata")
