# mainExample.R
# Albert Ziegler, Semmle, 2019

# Step for step file to reproduce the experiment 
# of improving the Wolfram ImageIdentify Net V1
# applied to downsized versions of insects
# using unsupervised recalibration

#### Constants ####

# turn off to skip non-essential parts
verbose = TRUE


#### Preparation ####

# Load the wolfram results
all_data <- read_in_wolfram_output(wolfram_output_file)

# Optional: See an example
if(verbose) 
  all_data %>% 
  plot_ImageIdentify_predictions("aaa417c0a850c136a3daa765bf7ca9ae")


#### Main part #### 