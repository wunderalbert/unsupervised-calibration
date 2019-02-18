# minimalExample.R
# Albert Ziegler, Semmle, 2019

# An example of making up data and a classifier,
# and performing unsupervised recalibration



##### Beforehand #####

library(tidyverse)
source("unsupervisedCalibration.R")

# How should we model that the prediction depends on the truth?
# Note that the example here is not even calibrated on the training set.
prediction <- function(truth) runif(length(truth), 
                                    ifelse(truth, runif(length(truth), 0, .5), 0),
                                    ifelse(truth, 1, runif(length(truth), .5, 1)))
# How many partitions are we going to use?
n_partitions <- 4




##### In the lab #####

n_lab <- 100000
base_rate_lab <- .5

truth_lab <- rbernoulli(n_lab, base_rate_lab)
pred_lab <- prediction(truth_lab)

ctp = new("ClassifierTrainingPerformance", pred_lab, truth_lab, n_partitions)





##### In the field ######

n_field = 10000
base_rate_field <- .2

truth_field <- rbernoulli(n_field, base_rate_field)
pred_field <- prediction(truth_field)

base_rate_field_detected <- unsupervised_calibration_get_base_rate(pred_field, ctp)
pred_posterior <- unsupervised_calibration_apply_base_rate(pred_field, base_rate)





##### Output #####

cat("In the lab, the base rate was", base_rate_lab %>% round(3), "\n")
cat("In the field, the base rate was", base_rate_field %>% round(3), "\n")
cat("The average prediction was", pred_field %>% mean %>% round(3), "\n")
cat("Unsupervised recalibration calculated the base rate as", base_rate_field_detected %>% round(3), "instead", "\n")

cat("For binary classification, unsupervised recalibration changed the accuracy from", 
    mean(truth_field == (pred_field > .5)) %>% round(2) * 100, 
    "% to ",
    mean(truth_field == (pred_posterior > .5)) %>% round(2) * 100, 
    "%", "\n")
