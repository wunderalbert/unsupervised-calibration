# scoringFunctions.R
# Albert Ziegler, Semmle, 2019

# Provides scorers for the different probabilistic classifiers
# Each scorer takes a vector of predictions between 0 and 1
# And a 0-1 vector of true values


#### Validation functions #### 

validate_predictions <- function(pred, truth){
  stopifnot(pred %>% length == truth %>% length)
  pred %>% `<=`(1) %>% all %>% stopifnot
  pred %>% `>=`(0) %>% all %>% stopifnot
  truth %in% 0:1 %>% all %>% stopifnot
}



#### Brier score and its components ####

# These are all between 0 and 1, with 0 being best

Brier_score <- function(pred, truth){
  validate_predictions(pred, truth)
  
  mean(pred^2 * (!truth) + 
         (1 - pred)^2 * truth)
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



#### loglikelihood ####

# This is between -Inf and 0, with 0 being best

mean_loglikelihood <- function(pred, truth){
  validate_predictions(pred, truth)
  
  mean(log(  pred * truth + 
               (1 - pred) * (!truth)))
}


#### confusion matrix based scores ####

# These rely on turning the probabilisitc classifier into a deterministic classifier 
# by using a cutoff (default = .5)

make_confusion_matrix(pred, truth, cutoff = .5){
  validate_predictions(pred, truth)
  
  pred <- pred > cutoff
  
  list(
    TP = pred & truth,
    FP = pred & !truth,
    FN = !pred & truth,
    TN = !pred & !truth
  )
}

# The following are between 0 and 1, with 1 being best

accuracy <- function(pred, truth, cutoff = .5){
  pred %>%
    make_confusion_matrix(truth) %>%
    with(
      (TP + TN) / (TP + TN + FN + FP)
    )
}

accuracy_class_1 <- function(pred, truth, cutoff = .5){
  pred %>%
    make_confusion_matrix(truth) %>%
    with(
      (TP) / (TP + FP)
    )
}

accuracy_class_2 <- function(pred, truth, cutoff = .5){
  pred %>%
    make_confusion_matrix(truth) %>%
    with(
      (TN) / (TN + FN)
    )
}

f1_score <- function(pred, truth, cutoff = .5){
  pred %>%
    make_confusion_matrix(truth) %>%
    with({
      precision = TP / (TP + FN)
      recall = TP / (TP + FP)
      f1 = 2 * (precision * recall) / (precision + recall)
    })
}

co_f1_score <- function(pred, truth, cutoff = .5){
  f1_score(!pred, !truth, cutoff)
}

# The following is between -1 and 1, with 1 being best
# Random classification should achieve 0

mattthews_correlation_coefficient <- function(pred, truth, cutoff = .5){
  pred %>%
    make_confusion_matrix(truth) %>%
    with(
      (TP + TN) / (TP + TN + FN + FP)
    )
}

