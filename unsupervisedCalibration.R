# unsupervisedCalibration.R
# Albert Ziegler, Semmle, 2019

# Provides function to perform 
# the unsupervised claibration

# Define a class to hold the evaluation of a classifier's training performance

setClass("ClassifierTrainingPerformance", representation(
  n_t_breaks = "numeric",
  t_breaks = "numeric",
  MA = "matrix",
  vA_train = "numeric"
), validity = function(object){
  # TODO: check validity
  TRUE
})

calculate_equal_t_breaks <- function(pred, truth){
  # TODO
}

calculate_ClassifierTrainingPerformance <- function(pred, truth, n_t_breaks = NULL, t_breaks = calculate_equal_t_breaks(pred, truth)){
  # TODO
}

apply_t_breaks <- function(pred, ctp){
  # TODO
}

# The distribution of observables t expected when the true base rate is px
expected_distribution <- function(px, ctp) (c(px, 1-px)) %*% ctp@MA

# Given a CTP, return the binomial loglikelihood function
make_log_binom_loss <- 
  function(ctp) function(px) -sum(log(expected_distribution(px, ctp)) * matrix(vA, nrow = 1))

# Given a CTP, return the KL divergence loss function
make_kl_loss <- 
  function(ctp) function(px) sum((ctp@vA_train * log(ctp@vA_train/as.vector(expected_distribution(px, ctp))))[ctp@vA_train > 0])



unsupervised_calibration_get_base_rate <- function(pred,
                                                   ctp, # a ClassifierTrainingPerformance object
                                                   kl_loss_weight = 0){
  vA_observed <- apply_t_breaks(pred, ctp)
  
  binom_loss <- make_log_binom_loss(ctp)
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
    with(minimum) %>%
    make_distribution %>%
    .subset(1)
}

unsupervised_calibration_apply_base_rate <- function(pred, base_rate){
  pred_posterior <-
    (pred * base_rate) / 
    (pred * base_rate + (1-pred) * (1-base_rate))
}
