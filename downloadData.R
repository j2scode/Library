## ---- download_data
#==============================================================================#
#                                 downloadData                                 #
#==============================================================================#
#'  downloadData
#' 
#' This takes as parameters, a list containing the directory to which a file 
#' should be stored, a filename and the URL from which the file will be 
#' downloaded.  The function downloads the data to the directory and filename
#' designated
#' 
#' @param downloadFile - a list containing the directory, filename and URL
#' 
#' @author John James
#' @export
downloadData <- function(url, filePath) {
  
  checkDir(filePath)
  
  download.file(url, destfile = filePath, mode = 'wb')
}

## ---- end