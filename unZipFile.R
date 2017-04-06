## ---- unzip_file

#==============================================================================#
#                                 unZipFile                                    #
#==============================================================================#
#'  unZipFile
#' 
#' This takes as parameters, a list containing the directory, filename, and
#' zipped file names and a directory into which the files should be unzipped.
#' The function unzips the files into the designated directory
#' 
#' @param zipFile - list containing the directory, file name, and zipped file
#'                  names
#' @param directory - the directory into which the unzipped files will be
#'                  stored                  
#' @author John James
#' @export
unZipFile <- function(zipFile, directory) {
  
  checkDir(directory)

  if (file.exists(zipFile$filePath)) {
    unzip(zipfile = zipFile$filePath, overwrite = FALSE, exdir = directory,
          junkpaths = TRUE, files = zipFile$files)
  } 
}

## ---- end
