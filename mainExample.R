# mainExample.R
# Albert Ziegler, Semmle, 2019

# Step for step file to reproduce the experiment 
# of improving the Wolfram ImageIdentify Net V1
# applied to downsized versions of insects
# using unsupervised recalibration



#### Constants ####

# turn off to skip non-essential parts and plots
verbose <- TRUE

# Wolfram does not publish the performance of its classifier on its training set
# So we use part of our own dataset (but not downsized!) to evaluate it
n_examples_reserved_for_evaluation <- 200

initial_seed <- 1

resolutions_to_test <- c(30, 40, 50, 75, 100, 200)
n_experiments_per_resolution <- 3

n_partitions_for_unsupervised_calibration <- 4





#### Preparation ####

# Load the wolfram results
# This will fail if the Wolfram scripts have not been run (with instructions on how to proceed)
all_data <- read_wolfram_output(expected_resolutions = resolutions_to_test)

# Optional: get summary statistics
if(verbose)
  cat("Read in", 
      all_data %>% subset(ground_truth_beetles) %>% with(filename) %>% unique %>% length, 
      "samples of beetles and",
      all_data %>% subset(!ground_truth_beetles) %>% with(filename) %>% unique %>% length, 
      "samples of butterflies.")

# Optional: see an example
if(verbose) 
  all_data %>% 
  plot_ImageIdentify_predictions("aaa417c0a850c136a3daa765bf7ca9ae")

# Since Wolfram does not open source its training set,
# we'll have to evaluate the performance of the classifier
# on a biased part of the iNaturalist set with full resolution
# Still, we need to be careful not to have it overlap with 
# the set we use for actual unsupervised recalibration.
mark_for_evaluation <- function(all_data, n_train) all_data %>%
  group_by(ground_truth_beetles) %>%
  mutate(evaluation = filename %in% sample(filename %>% unique, n_train)) %>%
  ungroup


#### Define evaluation function #### 

const <- function(x) {
  candidate <- x %>% unique
  candidate %>% is_scalar_vector %>% stopifnot()
  candidate
}

# Run the unsupervised recalibration several times for each resolution and
# collect several scores. 
# This is the function that will do it:

evaluate <- function(all_data, # all data requires columns image_size, filename, ground_truth_beetles and beetles
                               # if, in addition, it also has column "subpopulation", will perform recalibration by subpop
                     n_evaluation, 
                     image_size_used, 
                     n_partitions = n_partitions_for_unsupervised_calibration,
                     seed = NA){
  # evaluate the performance of unsupervised recalibration
  
  
  if (!is.na(seed)) set.seed(seed)
  
  current_data <- all_data %>% 
    mark_for_evaluation(n_evaluation)
  
  # evaluate the performance of the classifier on 
  # an unbiased set of full resolution pictures
  # (as close an approximation to the training set as is available)
  ctp = current_data %>%
    subset(evaluation & is.infinite(image_size)) %>%
    with(
      new("ClassifierTrainingPerformance", beetles, ground_truth_beetles, n_partitions)
    )
  
  # Now forget the evaluation set
  current_data <- current_data %>%
    subset(image_size == image_size_used & !evaluation)
  
  # if there are no subpopulations specified, process all together
  if (!"subpopulation" %in% colnames(current_data)) current_data$subpopulation <- "default"
  
  # perform unsupervised calibration independently on each subpopulation
  current_data <-
    current_data %>%
    group_by(subpopulation) %>%
    mutate(base_rate_beetle_detected = beetles %>% unsupervised_calibration_get_base_rate(ctp),
           beetles_posterior = beetles %>% unsupervised_calibration_apply_base_rate(base_rate_beetle_detected %>% const))
  
  base_rate_beetle_detected <- current_data %>%
    with(base_rate_beetle_detected) %>%
    mean
  
  cat(".")
  
  with(current_data, 
       {
         data.frame(stringsAsFactors = FALSE,
           
           # Parameters
           n_evaluation = n_evaluation,
           image_size_used = image_size_used,
           n_partitions = n_partitions,
           seed = seed,
           
           # Results
           raw_base_rate = beetles %>% mean,
           recalibrated_base_rate = base_rate_beetle_detected %>% mean,
           actual_base_rate = ground_truth_beetles %>% mean,
           
           # Scores
           raw_Brier_score = score_Brier(beetles, ground_truth_beetles),
           recalibrated_Brier_score = score_Brier(beetles_posterior, ground_truth_beetles),
           
           raw_Brier_calibration_score = score_Brier_calibration(beetles, ground_truth_beetles),
           recalibrated_Brier_calibration_score = score_Brier_calibration(beetles_posterior, ground_truth_beetles),
           
           raw_Brier_refinement_score = score_Brier_refinement(beetles, ground_truth_beetles),
           recalibrated_Brier_refinement_score = score_Brier_refinement(beetles_posterior, ground_truth_beetles),
           
           raw_negative_loglikelihood = score_mean_loglikelihood(beetles, ground_truth_beetles),
           recalibrated_negative_loglikelihood = score_mean_loglikelihood(beetles_posterior, ground_truth_beetles),
           
           raw_accuracy = score_accuracy(beetles, ground_truth_beetles),
           recalibrated_accuracy = score_accuracy(beetles_posterior, ground_truth_beetles)
         )
       })
}



#### Experiment 1: How well does unsupervised calibration perform for different resolutions ####

results_experiment_1 <- NULL
for (i in (1:n_experiments_per_resolution) - 1 + initial_seed){
  results_experiment_1 <- expand.grid(n_evaluation = n_examples_reserved_for_evaluation,
                                      image_size_used = resolutions_to_test,
                                      n_partitions = n_partitions_for_unsupervised_calibration,
                                      seed = i) %>%
    with(mapply(
      FUN = evaluate, 
      n_evaluation = .$n_evaluation, 
      image_size_used = .$image_size_used, 
      n_partitions = .$n_partitions, 
      seed = .$seed,
      SIMPLIFY = FALSE,
      MoreArgs = list(all_data = all_data))) %>%
    do.call(what = rbind) %>%
    rbind(results_experiment_1)
  
  if (verbose) 
    results_experiment_1 %>%
    plot_accuracy %>%
    plot
}

if (verbose) 
  results_experiment_1 %>%
  plot_accuracy(T)

if (verbose) 
  results_experiment_1 %>%
  plot_Brier_composition(F)

