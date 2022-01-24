#' Balanced subsample of all pairwise comparisons
#'
#' To reduce computational complexity of DQF method
#' @param n.osb integer number of data point
#' @param subsample number of pairs each observation shows up in
#' @keywords dqf
#' @export 
#' @examples 
#' subsamp.dqf(5,2)

subsamp.dqf <- function(n.obs, subsample) {
  # called by dqf.outlier, computes random subset of pairs
  pairs <- c()
  subsample <- floor(subsample/2)*2
  for (i in 1:n.obs) {
    for (j in (i+1):(i+subsample/2)) {
      pairs <- rbind(pairs, c(i,j*(j <= n.obs) + (j-n.obs)*(j > n.obs)))
    }
  }
  return(pairs)
}