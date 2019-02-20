# plotting.R
# Albert Ziegler, Semmle, 2019

# Provides visualisations for the 
# unsupervised recalibration experiments




plot_ImageIdentify_predictions <- function(data, names) {
  if (length(names) == 1) {
    data %>%
      subset(filename %>% str_detect(names)) %>%
      ggplot +
      geom_line(aes(x = image_size, y = butterflies)) + 
      scale_x_continuous("image size (largest dimension in pixels)") +
      scale_y_continuous("ImageIdentify chance for being a butterfly", labels = percent)
  } else {
    data %>%
      subset(filename %>% str_detect(names %>% paste0(collapse = "|"))) %>%
      ggplot +
      geom_line(aes(x = image_size, y = butterflies)) + 
      scale_x_continuous("image size (largest dimension in pixels)") +
      scale_y_continuous("ImageIdentify chance for being a butterfly", labels = percent)
  }
}


plot_accuracy <- function(results, show_different_partitions = T){
  results %>% 
    mutate(seed = paste0("Run ", seed),
           n_partitions = factor(n_partitions, 
                                 levels = n_partitions %>% unique %>% sort,
                                 labels = n_partitions %>% unique %>% sort %>% paste0(" partitions")),
           image_size_used = factor(image_size_used, 
                                    levels = image_size_used %>% unique %>% sort,
                                    labels = image_size_used %>% unique %>% sort %>% paste0(" pixels"))) %>%
    
    ggplot +
    
    (if (show_different_partitions) facet_wrap(~ n_partitions) else NULL) +
    
    stat_summary(aes(x = image_size_used,
                     y = recalibrated_accuracy,
                     fill = "after recalibration",
                     group = ""),
                 geom = "ribbon", fun.data = mean_se, alpha = .3) +
    stat_summary(aes(x = image_size_used,
                     y = recalibrated_accuracy,
                     col = "after recalibration",
                     group = ""),
                 geom = "line", fun.data = mean_se) +
    stat_summary(aes(x = image_size_used,
                     y = raw_accuracy,
                     col = "before recalibration",
                     group = ""),
                 geom = "line", fun.data = mean_se) +
    
    scale_color_manual("", 
                       values = c("blue", "red"),
                       aesthetics = c("fill", "colour")) +
    
    scale_y_continuous("accuracy", labels = percent) +
    xlab("image size")
}

plot_Brier_composition <- function(results, show_different_partitions = T){
  results %>%
    
    ggplot +
    
    (if (show_different_partitions) facet_wrap(~ n_partitions) else NULL) +
    
    stat_summary(aes(x = image_size_used, y = raw_Brier_calibration_score, col = "before recalibration"),
                 fun.data = mean_se) +
    stat_summary(aes(x = image_size_used, y = recalibrated_Brier_calibration_score, col = "after recalibration"),
                 fun.data = mean_se) +
    stat_summary(aes(x = image_size_used, y = raw_Brier_score, col = "before recalibration"),
                 fun.data = mean_se) +
    stat_summary(aes(x = image_size_used, y = recalibrated_Brier_score, col = "after recalibration"),
                 fun.data = mean_se) +
    
    stat_summary(aes(x = image_size_used, y = raw_Brier_score, col = "before recalibration", linetype = "Brier score"), 
                 geom = "line", fun.data = mean_se) +
    stat_summary(aes(x = image_size_used, y = recalibrated_Brier_score, col = "after recalibration", linetype = "Brier score"), 
                 geom = "line", fun.data = mean_se) +
    stat_summary(aes(x = image_size_used, y = raw_Brier_calibration_score, col = "before recalibration", linetype = "Brier calibration score"), 
                 geom = "line", fun.data = mean_se) +
    stat_summary(aes(x = image_size_used, y = recalibrated_Brier_calibration_score, col = "after recalibration", linetype = "Brier calibration score"),
                 geom = "line", fun.data = mean_se)+
    
    scale_linetype_manual("", values = c(`Brier score` = 1, `Brier calibration score` = 2), breaks = rev) +
    scale_color_manual("", values = c("blue", "red")) +
    
    scale_y_continuous("Brier score\n(0: best, 1: worst)") +
    scale_x_continuous("image size", labels = function(x) x %>% paste0(" pixels"))
}






