## ---- in_interval
#==============================================================================#
#                                  inInterval                                  #
#==============================================================================#
#'  inInterval
#' 
#' This function takes as its parameters, a number, an integer vector containing
#' the lower and upper bounds of the interval, and returns true if the number
#' is within the interval, false otherwise.
#' 
#' @param number - the number to evaluate
#' @param range - the integer vector containing the interval bounds
#' @return result - true if number is in the interval, inclusive, false otherwise
#' @author John James
#' @export
inInterval <- function(number, interval) {
  stopifnot(length(interval) == 2L)
  interval[1] <= number & number <= interval[2]
}
  