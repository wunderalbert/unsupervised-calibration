# plotting.R
# Albert Ziegler, Semmle, 2019

# Provides visualisations for the 
# unsupervised recalibration experiments

##### Preliminaries #####

mean_sd <- function(x) {
  x %>%
    (Hmisc::smean.sd) %>%
    t %>%
    as.data.frame %>%
    mutate(y = Mean, ymin = Mean - SD, ymax = Mean + SD)
}

mean_iq <- function(x, range = .95) {
  data.frame(
    y = mean(x),
    ymin = quantile(x, (1 - range) / 2),
    ymax = quantile(x, (1 + range) / 2)
  )
}

##### plotting functions #####

plot_ImageIdentify_predictions <- function(data, names, show_species = F) {
  data %>%
    ungroup %>%
    subset(filename %>% str_detect(names %>% paste0(collapse = "|"))) %>%
    mutate(image_size = 
             ifelse(is.infinite(image_size), "original", image_size) %>%
             as.character) %>%
    mutate(image_size = 
             factor(image_size,
                    levels = image_size[image_size %>% as.double %>% order(na.last = T)] %>% unique
             )) %>%
    ggplot(if (show_species) aes(col = species) else aes(col = NULL)) +
    geom_line(aes(x = image_size, y = 1 - beetles, 
                  group = filename)) + 
    geom_point(aes(x = image_size, y = 1 - beetles)) + 
    scale_color_discrete("species") +
    scale_x_discrete("image size (largest dimension in pixels)") +
    scale_y_continuous("predicted chance to be a butterfly", labels = percent,
                       sec.axis = sec_axis(trans = ~ ., "predicted chance to be a beetle", labels = function(x) percent(1-x)))
}


plot_accuracy <- function(results, show_different_partitions = results$n_partitions %>% unique %>% length > 1){
  results %>% 
    mutate(seed = paste0("Run ", seed),
           n_partitions = factor(n_partitions, 
                                 levels = n_partitions %>% unique %>% sort,
                                 labels = n_partitions %>% unique %>% sort %>% paste0(" partitions"))) %>%
    
    ggplot +
    
    (if (show_different_partitions) facet_wrap(~ n_partitions) else NULL) +
    
    stat_summary(aes(x = image_size_used,
                     y = recalibrated_accuracy,
                     col = "after recalibration",
                     group = ""),
                 geom = "line", fun.data = mean_iq) +
    stat_summary(aes(x = image_size_used,
                     y = recalibrated_accuracy,
                     col = "after recalibration",
                     group = ""), size = 2.2, 
                 geom = "point", fun.data = mean_iq) +
    stat_summary(aes(x = image_size_used,
                     y = recalibrated_accuracy,
                     fill = "after recalibration", col = NA,
                     group = ""), alpha = .25,
                 geom = "ribbon", fun.data = mean_iq,
                 show.legend = F) +
    
    stat_summary(aes(x = image_size_used,
                     y = raw_accuracy,
                     col = "before recalibration",
                     group = ""),
                 geom = "line", fun.data = mean_iq) +
    stat_summary(aes(x = image_size_used,
                     y = raw_accuracy,
                     col = "before recalibration",
                     group = ""), size = 2.2,
                 geom = "point", fun.data = mean_iq) +
    stat_summary(aes(x = image_size_used,
                     y = raw_accuracy,
                     fill = "before recalibration",  col = NA,
                     group = ""), alpha = .25,
                 geom = "ribbon", fun.data = mean_iq, 
                 show.legend = F) +
    
    scale_color_manual("", 
                       values = c("blue", "red"),
                       aesthetics = c("colour", "fill")) +
    
    scale_y_continuous("accuracy", labels = percent) +
    scale_x_continuous("image size", labels = function(x) x %>% paste0(" pixels"))
}


plot_accuracy_2 <- function(results,
                            show_different_resolutions = results$image %>% unique %>% length > 1){
    results %>% 
      mutate(seed = paste0("Run ", seed),
             n_partitions = factor(n_partitions, 
                                   levels = n_partitions %>% unique %>% sort,
                                   labels = n_partitions %>% unique %>% sort %>% paste0(" partitions")),
             image_size_used = factor(image_size_used, 
                                      levels = image_size_used %>% unique %>% sort,
                                      labels = image_size_used %>% unique %>% sort %>% paste0(" pixels"))) %>%
      
      ggplot +
    (if (show_different_resolutions) facet_wrap(~ image_size_used) else NULL) +
      stat_summary(aes(x = r_beetles,
                       y = recalibrated_accuracy,
                       col = "after recalibration",
                       group = ""),
                   geom = "line", fun.data = mean_iq) +
      stat_summary(aes(x = r_beetles,
                       y = raw_accuracy,
                       col = "before recalibration",
                       group = ""),
                   geom = "line", fun.data = mean_iq) +
      
      scale_color_manual("", 
                         values = c("blue", "red"),
                         aesthetics = c("fill", "colour")) +
      scale_y_continuous("accuracy", labels = percent) +
      scale_x_continuous("beetles in subpopulation 1", labels = percent,
                         sec.axis = sec_axis(trans = ~ ., "beetles in subpopulation 2", labels = function(x) percent(1-x)))
  }





plot_Brier_composition <- function(results, show_different_partitions = results$n_partitions %>% unique %>% length > 1){
  results %>%
    
    ggplot +
    
    (if (show_different_partitions) facet_wrap(~ n_partitions) else NULL) +
    
    stat_summary(aes(x = image_size_used, y = raw_Brier_calibration_score, fill = "before recalibration", col = NA),
                 geom = "ribbon", show.legend = F, alpha = .25,
                 fun.data = mean_iq) +
    stat_summary(aes(x = image_size_used, y = recalibrated_Brier_calibration_score, fill = "after recalibration", col = NA),
                 geom = "ribbon", show.legend = F, alpha = .25,
                 fun.data = mean_iq) +
    stat_summary(aes(x = image_size_used, y = raw_Brier_score, fill = "before recalibration", col = NA),
                 geom = "ribbon", show.legend = F, alpha = .25,
                 fun.data = mean_iq) +
    stat_summary(aes(x = image_size_used, y = recalibrated_Brier_score, fill = "after recalibration", col = NA),
                 geom = "ribbon", show.legend = F, alpha = .25,
                 fun.data = mean_iq) +
    
    stat_summary(aes(x = image_size_used, y = raw_Brier_score, col = "before recalibration", linetype = "Brier score", group = 1), 
                 geom = "line", fun.data = mean_iq) +
    stat_summary(aes(x = image_size_used, y = recalibrated_Brier_score, col = "after recalibration", linetype = "Brier score", group = 2), 
                 geom = "line", fun.data = mean_iq) +
    stat_summary(aes(x = image_size_used, y = raw_Brier_calibration_score, col = "before recalibration", linetype = "Brier calibration score", group = 3), 
                 geom = "line", fun.data = mean_iq) +
    stat_summary(aes(x = image_size_used, y = recalibrated_Brier_calibration_score, col = "after recalibration", linetype = "Brier calibration score", group = 4),
                 geom = "line", fun.data = mean_iq)+
    
    stat_summary(aes(x = image_size_used, y = raw_Brier_score, col = "before recalibration", linetype = "Brier score", group = 1), 
                 geom = "point", fun.data = mean_iq, size = 2.2)+
    stat_summary(aes(x = image_size_used, y = recalibrated_Brier_score, col = "after recalibration", linetype = "Brier score", group = 2), 
                 geom = "point", fun.data = mean_iq, size = 2.2)+
    stat_summary(aes(x = image_size_used, y = raw_Brier_calibration_score, col = "before recalibration", linetype = "Brier calibration score", group = 3), 
                 geom = "point", fun.data = mean_iq, size = 2.2)+
    stat_summary(aes(x = image_size_used, y = recalibrated_Brier_calibration_score, col = "after recalibration", linetype = "Brier calibration score", group = 4),
                 geom = "point", fun.data = mean_iq, size = 2.2)+
    
    scale_linetype_manual("", values = c(`Brier score` = 1, `Brier calibration score` = 2), breaks = rev) +
    scale_color_manual("", values = c("blue", "red"), aesthetics = c("colour", "fill")) +
    
    scale_y_continuous("Brier score\n(0: best, 1: worst)") +
    scale_x_continuous("image size", labels = function(x) x %>% paste0(" pixels"))
}




plot_Brier_composition_2 <- function(results, 
                                     show_different_resolutions = results$image %>% unique %>% length > 1,
                                     show_different_partitions = results$image_size_used %>% unique %>% length > 1
                                     ){
  results %>%
    
    ggplot +
    
    (if (show_different_partitions &! show_different_resolutions) facet_wrap(~ n_partitions) else NULL) +
    (if (show_different_resolutions) facet_wrap(~ image_size_used) else NULL) +

    stat_summary(aes(x = r_beetles, y = raw_Brier_score, col = "before recalibration", linetype = "Brier score"), 
                 geom = "line", fun.data = mean_iq) +
    stat_summary(aes(x = r_beetles, y = recalibrated_Brier_score, col = "after recalibration", linetype = "Brier score"), 
                 geom = "line", fun.data = mean_iq) +
    stat_summary(aes(x = r_beetles, y = raw_Brier_refinement_score, col = "before recalibration", linetype = "Brier refinement score"), 
                 geom = "line", fun.data = mean_iq) +
    stat_summary(aes(x = r_beetles, y = recalibrated_Brier_refinement_score, col = "after recalibration", linetype = "Brier refinement score"),
                 geom = "line", fun.data = mean_iq) +
    stat_summary(aes(x = r_beetles, y = raw_Brier_calibration_score, col = "before recalibration", linetype = "Brier calibration score"), 
                 geom = "line", fun.data = mean_iq) +
    stat_summary(aes(x = r_beetles, y = recalibrated_Brier_calibration_score, col = "after recalibration", linetype = "Brier calibration score"),
                 geom = "line", fun.data = mean_iq) +
    
    scale_linetype_manual("", values = c(`Brier score` = 1, `Brier refinement score` = 2, `Brier calibration score` = 3), breaks = rev) +
    scale_color_manual("", values = c("blue", "red")) +
    
    scale_y_continuous("Brier score\n(0: best, 1: worst)") +
    scale_x_continuous("beetles in subpopulation 1", labels = percent,
                       sec.axis = sec_axis(trans = ~ ., "beetles in subpopulation 2", labels = function(x) percent(1-x)))
}





