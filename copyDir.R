## ---- copy_dir
#==============================================================================#
#                               copyDir                                        #
#==============================================================================#
#'  copyDir
#' 
#' This function copies files from the 'from' directory to the 'to' directory
#' 
#' @param from- directory from which the  files are to be copied 
#' @param to - directory to which the  files are to be copied 
#' @author John James
#' @export
copyDir <- function(from, to) {
  
  # Load packages
  source('./config/schema.R')
  packages <- schema$dependencies$helperFunctions$copyDir
  lapply(packages, loadPackage)
  
  checkDir(to)
  
  sourceFiles <- list.files(path = from, full.names = TRUE)
  result <- lapply(seq_along(sourceFiles), function(x) {
       file.copy(from = sourceFiles[[x]], to = to, overwrite = TRUE)})
}
## ---- end
