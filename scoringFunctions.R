# plotting.R
# Albert Ziegler, Semmle, 2019

# Provides visualisations for the 
# unsupervised recalibration experiments

Brier_score <- function(pred, truth){
  mean(pred^2 * (1 - truth) + 
         (1 - pred)^2 * truth)
}

validate_predictions <- function(pred, truth){
  stopifnot(pred %>% length == truth %>% length)
  pred <= 1 %>% all %>% stopifnot
  truth <= 1 %>% all %>% stopifnot
  pred >= 0 %>% all %>% stopifnot
  truth >= 0 %>% all %>% stopifnot
}

Brier_calibration <- function(pred, truth, n_tiles = 10){
  validate_predictions(pred, truth)
  
  data.frame(pred, truth) %>%
    group_by(ntile(pred, n_tiles)) %>%
    summarize(r = mean((truth - mean(pred))^2),
              n = n()) %>%
    with(r*n) %>% 
    sum / length(pred)
}

Brier_refinement <- function(pred, truth, n_tiles = 10){
  validate_predictions(pred, truth)
  
  data.frame(pred, truth) %>%
    group_by(ntile(pred, n_tiles)) %>%
    summarize(m = mean(truth),
              r = m * (1-m),
              n = n()) %>%
    with(r*n) %>% 
    sum / length(pred)
}
