# unsupervisedCalibration.R
# Albert Ziegler, Semmle, 2019

# Provides function to perform  the unsupervised claibration

# A typical use would be:
# 
# 1. Evaluate your performance on the training set by
# ctp = new("ClassifierTrainingPerformance", pred, truth, n_partition_breaks)
# 
# 2. In the field, apply unsupervised calibration by
# base_rate = unsupervised_calibration_get_base_rate(pred, ctp)
# pred_posterior = unsupervised_calibration_apply_base_rate(pred, base_rate)

#### ClassifierTrainingPerformance (define and compute) ####

calculate_equal_partition_breaks <- function(pred, n){
  stopifnot(length(n) == 1, 
            n == round(n),
            n > 1)
  
  quantiles <-
    quantile(pred, 
             seq(0, 1, length.out = n + 1))
  quantiles[1] <- 0
  quantiles[n+1] <- 1
  
  quantiles
}

# applies same t breaks as ctp
apply_partition <- function(pred, ctp){
  pred %>% cut(ctp@partition_breaks) %>% as.factor %>% as.double
}

partition_histogram <- function(pred, ctp){
  pred %>% apply_partition(ctp) %>% factor(levels = 1:ctp@n_partition_breaks) %>% table
}

# Define a class to hold the evaluation of a classifier's training performance
setClass("ClassifierTrainingPerformance", representation(
  n_partition_breaks = "numeric",
  partition_breaks = "numeric", # we only consider partitions into intervals here
  MA = "matrix",
  vA_train = "numeric"
), validity = function(object){
  # TODO: check validity
  TRUE
})

norm_to_1 <- function(x) x / sum(x)
  
setMethod("initialize", 
          "ClassifierTrainingPerformance", 
          function(.Object, 
                   pred, 
                   truth, 
                   n_partition_breaks = NULL, # convenience functionality: can supply this and get equal partition breaks computed
                   partition_breaks = calculate_equal_partition_breaks(pred, n_partition_breaks)
          ){
            .Object@partition_breaks <- partition_breaks
            
            .Object@n_partition_breaks <- 
              length(partition_breaks) - 1
            
            .Object@MA <- rbind(
              pred[truth] %>% 
                partition_histogram(.Object) %>%
                norm_to_1,
              pred[!truth] %>% 
                partition_histogram(.Object) %>%
                norm_to_1
            )
            
            .Object@vA_train <- .Object@MA %>% colSums
            
            .Object
          })


#### Loss functions ####

# The distribution of observables t expected when the true base rate is px
make_distribution <- function(px) c(px, 1-px)
expected_distribution <- function(px, ctp) (make_distribution(px)) %*% ctp@MA

# Given a CTP, return the binomial loglikelihood function
make_log_binom_loss <- 
  function(ctp, vA_observed) function(px) -sum(log(expected_distribution(px, ctp)) * matrix(vA_observed, nrow = 1))

# Given a CTP, return the KL divergence loss function
make_kl_loss <- 
  function(ctp) function(px) sum((ctp@vA_train * log(ctp@vA_train/as.vector(expected_distribution(px, ctp))))[ctp@vA_train > 0])


#### Main ####

check_converged <- function(optimize_return){
  stopifnot(optimize_return$convergence == 0)
  optimize_return
}

unsupervised_calibration_get_base_rate <- function(pred, # the predictions in the field (possibly for a subpopulations)
                                                   ctp, # the classifier performance recorded during training
                                                   kl_loss_weight = 0 # relative weight for KL compared to binomial loglikelihood loss
){
  vA_observed <- partition_histogram(pred, ctp)
  
  binom_loss <- make_log_binom_loss(ctp, vA_observed)
  kl_loss <- make_kl_loss(ctp)
  
  loss_function <- 
    if (kl_loss_weight == 0) {
      binom_loss
    } else {
      function(px) binom_loss(px) +  kl_loss_weight * kl_loss(px)
    }
  
  base_rate <-
    loss_function %>%
    optimize(0:1) %>% 
    check_converged %>%
    with(minimum) %>%
    make_distribution %>%
    .subset(1)
}

unsupervised_calibration_apply_base_rate <- function(pred, base_rate){
  pred_posterior <-
    (pred * base_rate) / 
    (pred * base_rate + (1-pred) * (1-base_rate))
}
