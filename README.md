# Unsupervised Recalibration

This is example code for a post-processing method to improve already trained ML models. 

Unsupervised recalibration:

* does not need access to the inner workings of the ML model, since it does not retrain them.
* does not need access to ground truth, since it infers the most likely scenario from the imperfect ML model.
* does not need the ground truth distribution in the field to match that in the lab.

But unsupervised recalibration:

* does need access to the ML model's evaluation summaries during training time (or alternatively get a small set of training data with ground truth on which to observe the ML model).
* does need the ML classifier to be unbiased.

When applied globally, unsupervised recalibration improves the calibration of a classifier. When applied locally (i.e. by subpopulation for some relevant and new subpopulation the ML model is not biased towards), this also improves the classifier's refinement.

## Usage

The code is meant to be run in an interactive R session.

It depends on the `tidyverse` and `rjson` packages being installed and relies on the setup performed in `.Rprofile`. When opening the code as an RStudio project, that setup is performed automatically. When using a different R setup, you need to `source(.Rprofile)`.

### Minimal example

An artificial and minimal example is contained in the file `minimalExample.R`. Some data is made up, a basic classifier trained, put into a new context, and recalibrated without needing access to the ground truth in the new context.

### Main example

A proper example using the iNaturalist dataset is available in the file `mainExample.R`. 
This relies on access to the classifier 
[Wolfram ImageIdentify Net V1](https://resources.wolframcloud.com/NeuralNetRepository/resources/Wolfram-ImageIdentify-Net-V1). 
The performance of this classifier on size-reduced pictures from the iNaturalist dataset is evaluated and improved in an unsupervised way.

Three steps are necessary in preparation:

1. Download and unzip the [iNaturalist](https://github.com/visipedia/inat_comp) datasets
([1](http://www.vision.caltech.edu/~gvanhorn/datasets/inaturalist/fgvc5_competition/categories.json.tar.gz),
[2](https://storage.googleapis.com/inat_data_2018_us/train2018.json.tar.gz),
[3](https://storage.googleapis.com/inat_data_2018_us/train_val2018.tar.gz))
into the folder `./data/iNaturalist`.
2. From R, run ```read_iNaturalist_data() %>% prepare_iNaturalist_data_for_processing_by_mathematica```.
3. Apply the Wolfram classifier script to some resolutions. E.g. from a Linux shell with Wolfram Mathematica installed, use the command ``` (./butterflies-beetles-no-resize.wls beetles/ ; ./butterflies-beetles-no-resize.wls butterflies/; ./butterflies-beetles.wls beetles/ 30 ; ./butterflies-beetles.wls beetles/ 40 ;  ./butterflies-beetles.wls beetles/ 50 ; ./butterflies-beetles.wls beetles/ 75 ; ./butterflies-beetles.wls butterflies/ 30 ; ./butterflies-beetles.wls butterflies/ 40 ;  ./butterflies-beetles.wls butterflies/ 50 ; ./butterflies-beetles.wls butterflies/ 75 ;  ./butterflies-beetles.wls butterflies/ 100; ./butterflies-beetles.wls butterflies/ 200 ; )&```.

## Comparison with other quantification methods

Unsupervised recalibration can be treated as another method in the field of _quantification_. Comparison with three standard methods (Classify and Count, Adjusted Classify and Count, Expectation Maximization) is presented in `quantification/` directory in a manner described in [Karpov, Porshnev, Rudakov](https://doi.org/10.18653/v1%2FS16-1025). For an overview of quantification methods see Karpov _et al._, [Saerens, Latinne, Decaestecker](https://pubmed.ncbi.nlm.nih.gov/11747533/), and [Tasche](https://arxiv.org/abs/1701.05512). 

## Contributions

This code was published for reproducibility of the results described in the article [Unsupervised Recalibration](https://arxiv.org/abs/1908.09157).

We are grateful if anyone finds bugs or problems and alerts us to them so we can correct them. But we are not looking to implement new features.
