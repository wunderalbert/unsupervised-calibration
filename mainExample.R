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

seed <- 1


#### Preparation ####

# reproducability
set.seed(seed)

# Load the wolfram results
all_data <- read_in_wolfram_output(wolfram_output_file)

all_data <- all_data %>%
  make_evaluation_data(n_examples_reserved_for_evaluation)

# Optional: See an example
if(verbose) 
  all_data %>% 
  plot_ImageIdentify_predictions("aaa417c0a850c136a3daa765bf7ca9ae")



#### Main part #### 

