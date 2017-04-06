## ---- load_ngrams
#==============================================================================#
#                             loadNGrams                                       #
#==============================================================================#
#' loadNGrams
#' 
#' This function takes as its parameter, the meta data for a set of N-Grams,
#' loads them into memory and returns them in a list to the calling environment.
#' 
#' @param document - the meta data for the N-grams 
#' @return nGrams - R object loaded
#' @author John James
#' @export
loadNGrams <- function(document) {
  
  nGrams <- lapply(seq_along(document), function(x) {
    d <- loadObject(document[[x]])
    d
  })

  rm(list = ls()[grep("^trainingCorpus", ls())])
  
  return(nGrams)  
}
## ---- end