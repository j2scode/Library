## ---- prepareNGram

#==============================================================================#
#                                prepareNGram                                  #
#==============================================================================#
#'  prepareNGram
#' 
#' This function takes as its parameter, a document feature matrix, and the 
#' regex patterns, and creates a data frame containing the nGram, its front 
#' and back contexts and the nGram counts.
#' 
#' @param dfm - a document feature matrix
#' @param regex - the regex patterns
#' @return nGram - data table containing nGrams, front/back contexts and counts
#' @author John James
#' @export
prepareNGram <- function(dfm, regex) {
  
  startTime <- Sys.time()
  
  n = dfm@ngrams
  
  df <- data.frame(n = n, nGram = featnames(dfm),
                   front = sub(regex$splitters[[n]], "\\1", featnames(dfm)),
                   back = sub(regex$afterUnderscore, "\\1", featnames(dfm)),
                   c = colSums(dfm), row.names = NULL, 
                   stringsAsFactors = FALSE)
  
  # Convert to data table  
  nGram  <- as.data.table(df)
  setkey(nGram, nGram)
}
## ---- end