## ---- sample_data
#==============================================================================#
#                           sampleData                                         #
#==============================================================================#
#'  sampleData
#' 
#' This function takes as its parameters, a document in unlisted tokenized
#' format, the number of samples and the sample size and returns the samples
#' as a continguous vector of tokens
#' 
#' @param document - the document to be sampled
#' @param numChunks - the number of samples
#' @param chunkSize - the sample size in percent or number of tokens
#' @param format - format of the data to be returned values are:
#'            lv = list of character vectors
#'            ls = list of character strings
#'            v = unlisted character vector
#'            s = unlist character string
#' @param randomize = logical indicating whether the call should be randomized
#' @return samples - the samples in a contiguous vector of tokens
#' @author John James
#' @export
sampleData <- function(document, numChunks, chunkSize, format = 'lv',
                       randomize = FALSE) {
  
  # Initialize Seed
  if (randomize == FALSE) {
    set.seed(007)
  }
  
  # Validate inputs
  docLength <-length(document)
  stopifnot(numChunks * chunkSize <= docLength)
  
  # Sample Data
  start        <- sample(1:(docLength-chunkSize), numChunks)
  end          <- start + chunkSize - 1
  samples <- lapply(seq(1:numChunks), function(x) {
    document[start[x]:end[x]]
  })
  
  # Format output
  if (format == 'ls' | format == 's') {
    samples <- lapply(seq_along(samples), function(x) {
      paste(samples[[x]], collapse = ' ')
    })
  }
  
  if (format == 'v' | format == 's') {
    samples <- unlist(samples)
  }

  return(samples)
}
## ---- end