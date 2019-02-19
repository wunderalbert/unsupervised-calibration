# preparingData.R
# Albert Ziegler, Semmle, 2019

# Provides function to prepare the data
# to be sent to Wolfram Mathematica,
# and reads in the results.

# hardcoded paths
data_path_iNaturalist <- "./data/iNaturalist/"
data_path_wolfram <- "./data/wolfram/"

# better error messages:
stopifnot_with_message <- function(..., message){
  tryCatch(stopifnot(...),
           error = function(e){
             stop(message)
           })
}

read_iNaturalist_data <- function(){
  # This function reads in the iNaturalist database
  
  file_cats <- file.path(data_path_iNaturalist, "categories.json")
  file_train <- file.path(data_path_iNaturalist, "train2018.json")
  
  file_cats %>%
    file.exists %>% 
    stopifnot_with_message(message = "iNaturalist data categories.json not found. Maybe the file path is wrong?")
  file_train %>% 
    file.exists %>% 
    stopifnot_with_message(message = "iNaturalist data train2018.json not found. Maybe the file path is wrong?")
  
  cats2018 <- fromJSON(file = file_cats)
  train2018 <- fromJSON(file = file_train)
  
  categories <- 
    data.frame(
      category_id = cats2018 %>% sapply(function(x) x$id),
      order = cats2018 %>% sapply(function(x) x$order),
      name = cats2018 %>% sapply(function(x) x$name))
  
  training_data <-
    data.frame(file_name = train2018[[2]] %>% sapply(function(x) x$file_name),
               id = train2018[[2]] %>% sapply(function(x) x$id),
               id2 = train2018[[4]] %>% sapply(function(x) x$id),
               category_id = train2018[[4]] %>% sapply(function(x) x$category))
  
  stopifnot(all(training_data$id == training_data$id2))
  stopifnot(categories$category_id %>% max == training_data$category_id %>% max)
  
  training_data <- training_data %>%
    mutate(order =  categories$order[match(category_id, categories$category_id)],
           name =  categories$name[match(category_id, categories$category_id)])
  
  training_data %>% with(order) %>% is.na %>% any %>% `!` %>% 
    stopifnot_with_message(message = "Not all samples could be associated with a biological order.")
  
  iNaturalist_data <-
    training_data %>%
    subset(order %in% c("Lepidoptera", "Coleoptera")) %>%
    mutate(beetle = order == "Coleoptera")
}



prepare_iNaturalist_data_for_processing_by_mathematica <- function(iNaturalist_data){
  # this function copies some files into the wolfram folder!
  # returns a vector c(n_beetles, n_butterflies) with the number of processed examples
  
  beetle_files <- iNaturalist_data %>% 
    subset(beetle) %>%
    with(file_name) %>% 
    str_extract("Insecta.*")
  beetle_files <- file.path(data_path_iNaturalist, beetle_files)
  location_beetles <- file.path(data_path_wolfram, "beetles/")
  
  file.copy(from=beetle_files, to=location_beetles, 
            overwrite = TRUE, recursive = FALSE)
  
  butterfly_files <- iNaturalist_data %>% 
    subset(!beetle) %>% 
    with(file_name) %>%
    str_extract("Insecta.*")
  butterfly_files <-  file.path(data_path_iNaturalist, butterfly_files)
  location_butterfly <- file.path(data_path_wolfram, "butterflies/")
  
  file.copy(from=butterfly_files, to=location_butterfly, 
            overwrite = TRUE, recursive = FALSE, 
            copy.mode = TRUE)
  
  c(
    n_beetles = beetle_files %>% length,
    n_butterflies = butterfly_files %>% length
  )
}



read_wolfram_output <- function(number_of_expected_examples = NULL,
                                expected_resolutions = NULL){
  # Read in the butterflies_*.csv and beetles_*.csv computed by Mathematica
  
  # How to read in one file
  readFolder <- function(fromPath){
    fromPath %>%
      list.files(full.names = T, pattern = "b*.csv") %>%
      lapply(function(x) 
        x %>%
          read.csv %>% 
          mutate(any = beetles + butterflies,
                 beetles = beetles / any, butterflies = butterflies / any,
                 ground_truth_beetles = x %>% str_split("/", simplify = T) %>% as.vector %>% tail(2) %>% str_detect("beetles") %>% any,
                 image_size = str_extract(x, "_[\\d]+") %>% str_sub(2, -1) %>% as.double,
                 image_size = ifelse(is.na(image_size), Inf, image_size) %>% as.double)) %>%
      do.call(what = rbind)
  }
  
  # Read in all files
  
  wolfram_output <- 
    rbind(
      file.path(data_path_wolfram, "beetles") %>%
        readFolder,
      file.path(data_path_wolfram, "butterflies") %>%
        readFolder
    )
  
  # Validate
  
  print_warning <- function(type_missing){
    warning("No processed ", type_missing, " files found. Have you run Mathematica?", "\n",
            "To do so, prepare data with", "\n", 
            "> read_iNaturalist_data() %>% prepare_iNaturalist_data_for_processing_by_mathematica", "\n",
            "Then run in shell a commmand like", "\n", 
            "> (", 
            "./butterflies-beetles-no-resize.wls beetles/ ; ./butterflies-beetles-no-resize.wls butterflies/",
            "./butterflies-beetles.wls beetles/ 30 ; ./butterflies-beetles.wls beetles/ 40 ;  ./butterflies-beetles.wls beetles/ 50 ; ./butterflies-beetles.wls beetles/ 75 ;  ./butterflies-beetles.wls beetles/ 100; ./butterflies-beetles.wls beetles/ 200 ; ",
            "./butterflies-beetles.wls butterflies/ 30 ; ./butterflies-beetles.wls butterflies/ 40 ;  ./butterflies-beetles.wls butterflies/ 50 ; ./butterflies-beetles.wls butterflies/ 75 ;  ./butterflies-beetles.wls butterflies/ 100; ./butterflies-beetles.wls butterflies/ 200 ; ",
            ")&", "\n", 
            "(This step may take a while.)"
    )
  }
  
  if (wolfram_output %>% subset(ground_truth_beetles) %>% nrow == 0){
    print_warning("butterflies")
  }
  
  if (wolfram_output %>% subset(!ground_truth_beetles) %>% nrow == 0){
    print_warning("beetles")
  }
  
  if (number_of_expected_examples %>% length == 1){
    wolfram_output %>%
      group_by(image_size) %>% 
      summarize(n=n()) %>% 
      with(n == number_of_expected_examples) %>%
      all %>% 
      stopifnot_with_message(message = "The expectation for the number of examples in the data read in from Wolfram was not met.")
  }
  
  if (number_of_expected_examples %>% length == 2){
    wolfram_output %>%
      group_by(ground_truth_beetles, image_size) %>% 
      summarize(n=n()) %>% 
      with(ifelse(ground_truth_beetles, 
                  n == number_of_expected_examples["n_beetles"], 
                  n == number_of_expected_examples["n_butterflies"])) %>% 
      all %>%
      stopifnot_with_message(message = "The expectation for the number of examples in the data read in from Wolfram was not met.")
  }
  
  if (expected_resolutions %>% length > 1){
    expected_resolutions %>%
      c(Inf) %in% 
      all_data$image_size %>%
      all %>%
      stopifnot_with_message(message = "The expectation for the processed image sizes in the data read in from Wolfram was not met.")
  }
  
  # Return
  
  wolfram_output
}