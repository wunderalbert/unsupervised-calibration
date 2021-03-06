# scoringFunctions.R
# Albert Ziegler, Semmle, 2019

# Provides scorers for the different probabilistic classifiers
# Each score'r is called 's name starts with "score_"
# It takes a vector of predictions between 0 and 1
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

score_Brier <- function(pred, truth){
  validate_predictions(pred, truth)
  
  mean((pred-truth)^2)
}

score_Brier_calibration <- function(pred, truth, n_tiles = 10){
  validate_predictions(pred, truth)
  
  data.frame(pred, truth) %>%
    group_by(ntile(pred, n_tiles)) %>%
    summarize(r = (mean(truth) - mean(pred))^2,
              n = n()) %>%
    with(r*n) %>% 
    sum / length(pred)
}

score_Brier_refinement <- function(pred, truth, n_tiles = 10){
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

score_mean_loglikelihood <- function(pred, truth, weights = rep(1, length(truth))){
  validate_predictions(pred, truth)
  
  weighted.mean(log(  pred * truth + 
               (1 - pred) * (!truth)), weights)
}


#### confusion matrix based scores ####

# These rely on turning the probabilisitc classifier into a deterministic classifier 
# by using a cutoff (default = .5)

make_confusion_matrix <- function(pred, truth, cutoff = .5){
  validate_predictions(pred, truth)
  
  pred <- pred > cutoff
  
  list(
    TP = pred & truth,
    FP = pred & !truth,
    FN = !pred & truth,
    TN = !pred & !truth
  ) %>% 
    lapply(sum)
}

# The following are between 0 and 1, with 1 being best

score_accuracy <- function(pred, truth, cutoff = .5){
  pred %>%
    make_confusion_matrix(truth) %>%
    with(
      (TP + TN) / (TP + TN + FN + FP)
    )
}

score_precision_class_1 <- function(pred, truth, cutoff = .5){
  pred %>%
    make_confusion_matrix(truth) %>%
    with(
      (TP) / (TP + FP)
    )
}

score_precision_class_2 <- function(pred, truth, cutoff = .5){
  pred %>%
    make_confusion_matrix(truth) %>%
    with(
      (TN) / (TN + FN)
    )
}

score_recall_class_1 <- function(pred, truth, cutoff = .5){
  pred %>%
    make_confusion_matrix(truth) %>%
    with(
      (TP) / (TP + FN)
    )
}

score_recall_class_2 <- function(pred, truth, cutoff = .5){
  pred %>%
    make_confusion_matrix(truth) %>%
    with(
      (TN) / (TN + FP)
    )
}

score_f1_score <- function(pred, truth, cutoff = .5){
  pred %>%
    make_confusion_matrix(truth) %>%
    with({
      precision = TP / (TP + FN)
      recall = TP / (TP + FP)
      f1 = 2 * (precision * recall) / (precision + recall)
    })
}

score_co_f1_score <- function(pred, truth, cutoff = .5){
  f1_score(!pred, !truth, cutoff)
}

# The following is between -1 and 1, with 1 being best
# Random classification should achieve 0

score_mattthews_correlation_coefficient <- function(pred, truth, cutoff = .5){
  pred %>%
    make_confusion_matrix(truth) %>%
    with(
      (TP + TN) / (TP + TN + FN + FP)
    )
}

