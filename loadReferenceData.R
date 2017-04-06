## ---- load_reference_data
#==============================================================================#
#                            loadReferenceData                                 #
#==============================================================================#
#'  loadReferenceData
#' 
#' This function takes as its parameter, the reference file schema and returns
#' the reference files by name into the calling environment.
#' 
#' @param referenceFiles - reference file meta data
#' @return referenceData - a list containing the reference data
#' @author John James
#' @export
loadReferenceData <- function(referenceFiles) {
  
  # Obtain the names of the reference file objects 
  names <- lapply(seq_along(referenceFiles), function(x) {
    referenceFiles[[x]]$objName
  })
  
  # Read the reference data
  referenceData <- lapply(seq_along(referenceFiles), function(x) {
    y <- readFile(referenceFiles[[x]])
    assign(referenceFiles[[x]]$objName, y)})
  
  # Apply names to the reference data
  names(referenceData) <- names
  
  return(referenceData)
}
## ---- end