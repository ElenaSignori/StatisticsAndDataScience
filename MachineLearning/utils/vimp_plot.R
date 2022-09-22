# funzione per variable importance plot
vimp_plot <- function(object) {
  # vimp_plot prende come argomento l'output della funzione ranger::ranger()
  
  if(class(object) != "ranger") {
    stop("L'argomento 'object' dev'essere di classe 'ranger'.")
  }
  
  if(is.null(object$variable.importance)) {
    stop("Elemento 'variable.importance' non trovato.")
  }
  
  variable_imp_df <- data.frame(
    var = names(object$variable.importance),
    importance = as.numeric(object$variable.importance)
  )
  
  variable_imp_df %>% 
    ggplot() +
    geom_col(aes(reorder(var, importance), importance, fill = var)) + 
    coord_flip() +
    theme(
      legend.position = "none"
    )
}