
# Define a class to hold the evaluation of a classifier's training performance
setClass("PlattScaling", representation(
  A = "numeric",
  B = "numeric"
), validity = function(object){
  (length(object@A) == 1) &
    (length(object@B) == 1)
})

setMethod("initialize", 
          "PlattScaling", 
          function(.Object, 
                   pred, 
                   truth, 
                   true_proportion_in_training_set = .5 # i.e. balanced. Use NA for "as obsorved"
          ){
            # Validate inputs#
            
            truth %>% assert_0_or_1
            pred %>% assert_between_0_and_1
            
            weights <- if (true_proportion_in_training_set %>% is.na) {
              1 %>% rep(truth %>% length)
            } else {
              truth %>%
                ifelse((1 - mean(truth)) * true_proportion_in_training_set,
                       mean(truth) * (1 - true_proportion_in_training_set))
            }
            
            optimization <- optim(c(0, 0), function(x) 
              pred %>% apply_platt_scaling(A = x[1], B = x[2]) %>% score_mean_loglikelihood(truth, weights) * (-1))
            
            stopifnot(optimization$convergence == 0)
            
            .Object@A = optimization$par[1]
            .Object@B = optimization$par[2]
            
            .Object
          })


apply_platt_scaling <-function(pred, 
                               ps, # supply either platt scaling object
                               A = ps@A, B=ps@B # or numbers A, B
){
  1 / (1 + exp(A * log(pred/(1-pred)) + B))
}