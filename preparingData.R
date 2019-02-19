# preparingData.R
# Albert Ziegler, Semmle, 2019

# Provides function to prepare the data
# to be sent to Wolfram Mathematica,
# and reads in the results.

# hardcoded paths
data_path_iNaturalist <- "./data/iNaturalist/"
data_path_wolfram <- "./data/wolfram/"

read_iNaturalist_data <- function(){
  file_cats <- file.path(data_path_iNaturalist, "categories.json")
  file_train <- file.path(data_path_iNaturalist, "train2018.json")
  
  file_cats %>% file.exists %>% stopifnot
  file_train %>% file.exists %>% stopifnot
  
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
  
  stopifnot(training_data %>% with(order) %>% is.na %>% any %>% `!`)
  
  iNaturalist_data <-
    training_data %>%
    subset(order %in% c("Lepidoptera", "Coleoptera")) %>%
    mutate(beetle = order == "Coleoptera")
}

prepare_iNaturalist_data_for_processing_by_mathematica <- function(iNaturalist_data){
  # this function copies some files into the wolfram folder!
  
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
}

read_wolfram_data <- function(path = wolfram_data){
  
}