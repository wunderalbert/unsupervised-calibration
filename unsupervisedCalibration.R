# unsupervisedCalibration.R
# Albert Ziegler, Semmle, 2019

# Provides function to perform  the unsupervised claibration
# of a binary classifier

# A typical use would be:
# 
# 1. In the lab, evaluate your performance on the training set by
# ctp = new("ClassifierTrainingPerformance", pred, truth, n_partition_breaks)
# 
# 2. In the field, apply unsupervised calibration by
# base_rate = unsupervised_calibration_get_base_rate(pred, ctp)
# pred_posterior = unsupervised_calibration_apply_base_rate(pred, base_rate)
#
# 2a) If you suspect that your training set may not be representative for
# your field set, apply unsupervised calibration overall.
#
# 2b) If alternatively or additionally you suspect a division into 
# subpopulations would be a meaningful distinction that the classifier
# does not already capture, apply unsupervised calibration
# independently to each subpopulation.




#### Preliminaries ####


norm_to_1 <- function(x) {
  # make a vector x >= 1 into a probability distribution (no checking)
  x / sum(x)
}

make_distribution <- function(px) {
  # make a base rate into a probability distribution (no checking)
  c(px, 1-px)
}

assert_between_0_and_1 <- function(x) {
  # check that all elements are between 0 and 1 (pass through input)
  stopifnot(all(0 <= x))
  stopifnot(all(x <= 1))
  x
}

assert_0_or_1 <- function(x){
  # check that all elements are 0 or 1 (pass through input)
  stopifnot(all(x %in% 0:1))
  x
}

check_converged <- function(optimize_return){
  # assert that the output of a call to optimize converged (pass through input)
  stopifnot(optimize_return$convergence == 0)
  optimize_return
}



#### ClassifierTrainingPerformance (define and compute) ####

# This section defines an object that holds
# the report of a classifier's observed performance 
# on the / a training set

calculate_equal_partition_breaks <- function(pred, n){
  # calculate partition into n breaks which are equally likely
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

apply_partition <- function(pred, ctp){
  # applies same partition as contained in the ctp
  pred %>% 
    cut(ctp@partition_breaks) %>% 
    as.factor %>% 
    as.double
}

partition_histogram <- function(pred, ctp){
  # compiles histogram of the values in a partition
  pred %>% 
    apply_partition(ctp) %>% 
    factor(levels = 1:ctp@n_partition_breaks) %>% 
    table
}

# Define a class to hold the evaluation of a classifier's training performance
setClass("ClassifierTrainingPerformance", representation(
  n_partition_breaks = "numeric",
  partition_breaks = "numeric", # we only consider partitions into intervals here
  MA = "matrix",
  vA_train = "numeric"
), validity = function(object){
  (length(object@partition_breaks) == 1 + object@n_partition_breaks) &
    (object@partition_breaks %>% max == 1) &
    (object@partition_breaks %>% min == 0) &
    (nrow(object@MA) == 2) &
    (ncol(object@MA) == object@n_partition_breaks) &
    (length(object@vA_train) == object@n_partition_breaks) &
    (max(object@vA_train) <= 1) &
    (min(object@vA_train) >= 0) &
    (max(object@MA) <= 1) &
    (min(object@MA) >= 0)
})

setMethod("initialize", 
          "ClassifierTrainingPerformance", 
          function(.Object, 
                   pred, 
                   truth, 
                   n_partition_breaks = NULL, # int > 1, convenience functionality: can supply this and get equal partition breaks computed
                   partition_breaks = NULL # this can be computed as equal breaks from n_partition_breaks
          ){
            # Construct a CTP from comparing predictions and truth on a training set.
            # Need to supply either explicit partition breaks or 
            # the number of partitions break of equal apparent probability to use.
            
            #### Validate inputs####
            
            truth %>% assert_0_or_1
            pred %>% assert_between_0_and_1
            
            # exactly one of n_partition_breaks and partition_breaks needs to be provided
            xor(n_partition_breaks %>% is.null, partition_breaks %>% is.null) %>% stopifnot
            if (partition_breaks %>% is.null) {
              partition_breaks <- calculate_equal_partition_breaks(pred, n_partition_breaks)
            }
            
            partition_breaks %>% assert_between_0_and_1
            partition_breaks %>% head(1) %>% unname %>% identical(0) %>% stopifnot
            partition_breaks %>% tail(1) %>% unname %>% identical(1) %>% stopifnot
            
            
            #### Compute fields ####
            
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

expected_distribution <- function(px, ctp){
  # The distribution of observables t expected when the true base rate is px
  (make_distribution(px)) %*% ctp@MA
} 

make_log_binom_loss <- function(ctp, vA_observed) {
  # Given a CTP, return the negative binomial loglikelihood function
  function(px) -sum(log(expected_distribution(px, ctp)) * matrix(vA_observed, nrow = 1))
}

make_kl_loss <- function(ctp) {
  # Given a CTP, return the KL divergence loss function
  function(px) sum((ctp@vA_train * log(ctp@vA_train/as.vector(expected_distribution(px, ctp))))[ctp@vA_train > 0])
  # subsetting at ctp@vA_train > 0 avoids the NaN at places where no hits are expected
}






#### Main ####

unsupervised_calibration_get_base_rate <- function(pred, # the predictions in the field (possibly for a subpopulations)
                                                   ctp, # the classifier performance recorded during training
                                                   kl_loss_weight = 0 # relative weight for KL compared to binomial loglikelihood loss
){
  # This is the main function to compute the unsupervised calibration
  # (in the sense that we compute the most likely base rate)
  # If unsupervised calibration is to be applied to different subpopulations,
  # process each subpopulation individually using this function.
  
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
  # Applied a known baserate to update predictions
  pred_posterior <-
    (pred * base_rate) / 
    (pred * base_rate + (1-pred) * (1-base_rate))
}
