## ---- read_file
#==============================================================================#
#                                 readFile                                     #
#==============================================================================#
#'  readFile
#' 
#' The function takes a directory and file name as s parameter, then reads the 
#' file and returns the object to the calling environment.
#' 
#' @param file - file name
#' @return directory - the directory containing the file
#' @author John James
#' @export

readFile <- function(object) {
  
  directory <- object$directory
  file      <- object$fileName
  
  if (file_ext(file) == 'txt' | file_ext(file) == 'dic') {
    fileData <- readLines(file.path(directory, file))
  } else {
    if (file_ext(file) == 'csv') {
      fileData <- read.csv(file.path(directory, file), 
                                header = TRUE,
                                stringsAsFactors = FALSE)
    }
  }
  
  return(fileData)
}
  
## ---- end