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


plot_accuracy(results, 
              show_ranges = NULL # use an interval c(a, b) with 0 < a, b < 1 for plotting quantile ranges
              ){
  results %>%
    subset(!bias_training_data) %>%
    subset(image_size_actual > 50) %>%
    group_by(image_size_actual, n_training, bias_training_data, image_size_training) %>%
    summarize(uninformed_accuracy_min = quantile(uninformed_accuracy, .25),
              informed_accuracy_min = quantile(informed_accuracy, .25),
              uninformed_accuracy_max = quantile(uninformed_accuracy, .75),
              informed_accuracy_max = quantile(informed_accuracy, .75),
              uninformed_accuracy = quantile(uninformed_accuracy, .50),
              informed_accuracy = quantile(informed_accuracy, .50),
              seed = "all") %>% #head %>% print
    ggplot +
    facet_grid(. ~ n_training) +
    scale_y_continuous("accuracy", labels = percent) +
    scale_color_discrete("") +
    # only add the ribbons if needed
    ifelse(length(show_ranges) == 2 & all(show_ranges <= 1) & all(show_ranges >=0),
           list(
             geom_ribbon(aes(x = image_size_actual, ymin = informed_accuracy_min, ymax = informed_accuracy_max, fill = "informed accuracy"), alpha = .2),
             geom_ribbon(aes(x = image_size_actual, ymin = uninformed_accuracy_min, ymax = uninformed_accuracy_max, fill = "uninformed accuracy"), alpha = .2),
             scale_fill_discrete("values and\ninterquartile ranges", aesthetics = c("fill", "col"))),
           NULL) +
    # recalibrated results
    geom_point(aes(shape = n_training %>% factor, x = image_size_actual, y = uninformed_accuracy, col = "uninformed accuracy")) +
    geom_line(aes(x = image_size_actual, y = uninformed_accuracy, col = "uninformed accuracy")) +
    # original results
    geom_point(aes(shape = n_training %>% factor, x = image_size_actual, y = informed_accuracy, col = "informed accuracy")) +
    geom_line(aes(x = image_size_actual, y = informed_accuracy, col = "informed accuracy", group = paste0(seed, n_training, image_size_training)))
}

