# Unsupervised Aggregation

example code for a post-processing method to improve already trained ML models

## Usage

The code is meant to be run in an interactive R session.

It depends on the `tidyverse` and `rjson` packages being installed and relies on the setup performed in `.Rprofile`. When opening the code as an RStudio project, that setup is performed automatically.

### Minimal example

An artificial and minimal example is contained in the file `minimalExample.R`.

### Main example

A proper example using the iNaturalist dataset is available in the file `mainExample.R`. 
This relies on access to the classifier 
[Wolfram ImageIdentify Net V1](https://resources.wolframcloud.com/NeuralNetRepository/resources/Wolfram-ImageIdentify-Net-V1). 
The performance of this classifier on size-reduced pictures from the iNaturalist dataset is evaluated and improved in an unsupervised way.

Three steps are necessary in preparation:

1. Download and unzip the [dataset](https://github.com/visipedia/inat_comp) into the folder `./data/iNaturalist`.
2. From R, run ```read_iNaturalist_data() %>% prepare_iNaturalist_data_for_processing_by_mathematica```.
3. Apply the Wolfram classifier. E.g. from a Linux shell with Wolfram Mathematica installed, use the command ``` (./butterflies-beetles-no-resize.wls beetles/ ; ./butterflies-beetles-no-resize.wls butterflies/; ./butterflies-beetles.wls beetles/ 30 ; ./butterflies-beetles.wls beetles/ 40 ;  ./butterflies-beetles.wls beetles/ 50 ; ./butterflies-beetles.wls beetles/ 75 ; ./butterflies-beetles.wls butterflies/ 30 ; ./butterflies-beetles.wls butterflies/ 40 ;  ./butterflies-beetles.wls butterflies/ 50 ; ./butterflies-beetles.wls butterflies/ 75 ;  ./butterflies-beetles.wls butterflies/ 100; ./butterflies-beetles.wls butterflies/ 200 ; )&```.

## Contributions

This code was published for reproducibility of the results described in the article "Unsupervised Recalibration".

We are grateful if anyone finds bugs or problems and alerts us to them so we can correct them. But we are not looking to implement new features.
