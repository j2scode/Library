## ---- save_file
#==============================================================================#
#                             saveFile                                         #
#==============================================================================#
#' saveFile
#' 
#' This function function takes as a parameter, file data and meta data and 
#' saves the file in accordance with the file meta data (directory, filename).
#' The function stores the data in plain text format. 
#' 
#' @param object - a list containing the file data and meta data.
#' @author John James
#' @export
saveFile <- function(object) {
  
  # Create directory if necessary
  checkDir(object$directory)

  # Assign file path 
  filePath  <- file.path(object$directory, object$fileName)
  
  # Save file
  if (file_ext(object$fileName) == 'txt' | file_ext(object$fileName) == 'dic') {
    con <- file(filePath)
    writeLines(object$data, con)
    close(con)
  } else {
    if (file_ext(object$fileName) == 'csv') {
      write.csv(x = object$data, file = filePath, row.names = FALSE)
    }
  }
}
## ---- end