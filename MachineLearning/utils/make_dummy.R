library(dplyr)
library(recipes)

make_dummy <- function(data, formula = formula(), to_dummy = character()) {
  features <- all.vars(formula) 
  
  response <- features[1]
  
  predictors <- features[2:length(features)]
  if(length(predictors) == 1 & predictors[1] == ".") {
    predictors <- setdiff(names(data), response)
  }
  
  data %>% 
    recipe(formula) %>%
    step_dummy(all_of(to_dummy), one_hot = F) %>%
    prep() %>%
    bake(data) %>%
    select(names(response), everything())
}