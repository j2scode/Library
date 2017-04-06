## ---- save_object
#==============================================================================#
#                             saveObject                                       #
#==============================================================================#
#' saveObject
#' 
#' This function function takes as a parameter, a list containing both data and 
#' meta data (directory, object name, filename), and saves itaccording to the 
#' the metaData.
#' 
#' @param object - R object to be saved
#' @return object - R object saved 
#' @author John James
#' @export
saveObject <- function(object) {
  
  # Unpack instructions
  directory  <- object$directory
  objName    <- object$objName
  fileName   <- object$fileName
  
  # Create directory if necessary
  checkDir(directory)
  
  # Assign analysis object to custom name 
  assign(objName, object$data)
  
  # Assign file path 
  filePath  <- file.path(directory, fileName)
  
  # Save file
  save(list=objName, file = filePath)
  
}

## ---- end