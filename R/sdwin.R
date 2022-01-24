#' Windosrized Standard Deviation Computation
#'
#' Computes the windorized standard deviation of a univariate data set
#' @param x univariate data vector
#' @param k integer number of points to be windorized from each extreme
#' @keywords dqf
#' @export 
#' @examples
#' sd.w(rnorm(100), 3)

sd.w <- function(x, k) {
  # computes the windsorized standard deviation, called by dqf.outlier
  # Inputs:
  ## k: (integer) number of observations at each extreme to alter
  ## x: (vector) numeric data values
  k <- floor(k)
  if (k == 0) {  #corresponds to non-robust adaptive DQF
    return(sd(x))
  } else {
    x <- sort(x)
    n <- length(x)
    x[1:k] <- x[k]
    x[(n-k+1):n] <- x[n-k+1]
    return(sd(x))
  }
}
