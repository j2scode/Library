## ---- logR
#==============================================================================#
#                                   logR                                       #
#==============================================================================#
#'  logR
#' 
#' This function takes as its parameter, a function, the start time, the file
#' containing the output and updates the log file.
#' 
#' @param functionName - name of the function
#' @param startTime - the time the function started in datetime format
#' @param directory - directory into which the data was saved
#' @param fileName - the filename into which data was saved
#' @return NA - loads the log into parent environment
#' @author John James
#' @export
logR <- function(functionName, startTime, directory, fileName) {
  
  # Stop the clock
  endTime  <- Sys.time()
  duration <- difftime(endTime, startTime, units = 'secs')
  
  # Save in Log
  newLog <- data.frame(functionName = functionName, 
                       startTime = startTime, endTime = endTime, 
                       duration = duration, directory = directory,
                       fileName = fileName)
  # Format log data
  log <- list()
  log$directory <- './logs'
  log$fileName  <- 'log.Rdata'
  log$objName   <- 'log'
  
  # Load if file exists, otherwise save new entry
  if (file.exists(file.path(log$directory, log$fileName))) {
    oldLog <- loadObject(log)
    log$data <- rbind(oldLog, newLog)
  } else {
    log$data <- newLog
  }
  saveObject(log)
  
}