## ---- load_object
#==============================================================================#
#                             loadObject                                       #
#==============================================================================#
#' loadObject
#' 
#' This  function takes an object definition as a parameter, the loads and 
#' returns the object to the calling environment 
#' 
#' @param object - the instructions used to load the R object
#' @return object - R object loaded
#' @author John James
#' @export
loadObject <- function(object) {
  
  # Unpack instructions
  directory  <- object$directory
  objName    <- object$objName
  fileName   <- object$fileName
  
  # Create file path
  filePath <- file.path(directory, fileName)
  
  if (file.exists(filePath)) {
  
    # load object
    env <- new.env()
    object <- load(filePath, envir = env)
    return(env[[object]])
  } else {
    return(FALSE)
  }
}
## ---- end