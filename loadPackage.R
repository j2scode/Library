## ---- load_package
#==============================================================================#
#                              loadPackage                                     #
#==============================================================================#
#'  loadPackage
#' 
#' This function takes as its parameter, a package to load, and installs the 
#' package, if necessary, then sources it into the current environment using 
#' the library function.
#' 
#' @param package - package to be loaded
#' @return result - NULL
#' @author John James
#' @export
loadPackage <- function(package) {
  
  if (package != 'openNLPmodels.en') {
    if (!package %in% installed.packages()) {
      install.packages(package, repos = "http://cran.us.r-project.org")
    }
  } else {
    if (!'openNLPmodels.en' %in% installed.packages()) {
      install.packages("openNLPmodels.en", 
                       repos = "http://datacube.wu.ac.at/", 
                       type = "source")
    }
  }
  
  suppressMessages(library(package, character.only=TRUE))
}
## ---- end