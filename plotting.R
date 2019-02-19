# plotting.R
# Albert Ziegler, Semmle, 2019

# Provides visualisations for the 
# unsupervised recalibration experiments




plot_ImageIdentify_predictions <- function(data, names) {
  if (length(names) == 1) {
    data %>%
      subset(filename %>% str_detect(name)) %>%
      ggplot +
      geom_line(aes(x = image_size, y = butterflies)) + 
      scale_x_continuous("image size (largest dimension in pixels)") +
      scale_y_continuous("ImageIdentify chance for being a butterfly", labels = percent)
  } else {
    data %>%
      subset(filename %>% str_detect(name %>% paste0(collapse = "|"))) %>%
      ggplot +
      geom_line(aes(x = image_size, y = butterflies)) + 
      scale_x_continuous("image size (largest dimension in pixels)") +
      scale_y_continuous("ImageIdentify chance for being a butterfly", labels = percent)
  }
}


plot_accuracy <- function(results){
  results %>% 
    mutate(seed = paste0("Run ", seed),
           n_partitions = factor(n_partitions, 
                                 levels = n_partitions %>% sort,
                                 labels = n_partitions %>% sort %>% paste0(" partitions")),
           image_size_used = factor(image_size_used, 
                                    levels = image_size_used %>% sort,
                                    labels = image_size_used %>% sort %>% paste0(" pixels"))) %>%
    ggplot +
    facet_wrap(~ n_partitions) +
    scale_y_continuous("accuracy", labels = percent) +
    stat_summary(aes(x = image_size_used,
                     y = recalibrated_accuracy,
                     fill = "after recalibration"),
                 geom = "ribbon", fun.data = mean_se, alpha = .3) +
    stat_summary(aes(x = image_size_used,
                     y = recalibrated_accuracy,
                     col = "after recalibration"),
                 geom = "line", fun.data = mean_se) +
    stat_summary(aes(x = image_size_used,
                     y = raw_accuracy,
                     col = "before recalibration"),
                 geom = "line", fun.data = mean_se) +
    scale_color_discrete("", 
                         values = c("red", "blue"),
                         aesthetics = c("fill", "colour"))
}

plot_Brier_composition <- function(results){
  rbind(
    results %>% mutate(what = "before calibration"),
    results %>% mutate(what = "after calibration")
  ) %>%
    group_by(image_size_used) %>%
    summarize(
      Brier_score = ifelse(what == "before calibration",
                           raw_Brier_score %>% mean,
                           recalibrated_Brier_score %>% mean), 
      calibration = ifelse(what == "before calibration",
                           raw_Brier_calibration_score %>% mean,
                           recalibrated_Brier_calibration_score %>% mean),
      refinement = ifelse(what == "before calibration",
                          raw_Brier_refinement_score %>% mean,
                          recalibrated_Brier_refinement_score %>% mean)
    ) %>%
    mutate(image_size_used = factor(image_size_used, 
                                    levels = image_size_used %>% sort,
                                    labels = image_size_used %>% sort %>% paste0(" pixels"))) %>%
    ggplot +
    facet_grid(what ~ image_size_used) +
    scale_y_continuous("Brier score", labels = percent) +
    geom_ribbon(aes(x = image_size_used, ymin = 0, ymax = refinement,
                    fill = "refinement"), alpha = .5) + 
    geom_ribbon(aes(x = image_size_used, ymin = refinement, ymax = refinement + calibration,
                    fill = "calibration"), alpha = .5) + 
    geom_line(aes(x = image_size_used, y = Brier_score,
                    col = "overall Brier score")) +
    scale_color_discrete("", 
                         values = c("red", "blue"),
                         aesthetics = c("fill", "colour"))
}