#' Interactive Visualization of DQF information
#'
#' Creates 3 interactive plots of DQFs with summarization on a 4th
#' @param dqfs a dqf.outlier object
#' @param show vector of preselected observation indicies to highlight
#' @keywords dqf 
#' @export 
#' @examples 
#' fit.dqf <- dqf.outlier(iris[51:102,1:4], g.scale=6)
#' dqf.explore(fit.dqf)
dqf.explore <- function(dqfs, show=c()) {
  # function to visualize depth quantile functions
  #
  # inputs:
  ## dqfs - object created by dqf.outlier
  ## show - vector of observations to highlight
  dqfs$dqf <- dqfs$dqf2
  click.points <- show
  n.qs <- ncol(dqfs$dqf)
  par(mfrow=c(1,1))
  y.hat <- matrix(0, nrow=nrow(dqfs$dqf), ncol=99)
  for (i in 1:nrow(dqfs$dqf)) {
    y <- dqfs$dqf[i,]/ max(dqfs$dqf[i,])
    x <- seq(0,1,length.out=length(y))
    y.hat[i,] <- diff(ksmooth(x,y,kernel="normal", band=.05, n.points=100)$y)
  }
  plot(c(0,1), c(0,max(dqfs$dqf/(nrow(dqfs$dqf/2)))), t='n', main="Select Observations - Press ESC when done", xlab="1 of 3", ylab="")
  for (i in 1:nrow(dqfs$dqf)) {
    lines(seq(0,1,length.out=100), dqfs$dqf[i,]/ (nrow(dqfs$dqf)/2), lwd=.3 + (sum(i==click.points)>0), col=2 + (sum(i==click.points)>0))
  } 
  points(cbind(seq(0,1,length.out=100),as.vector(t(dqfs$dqf/(nrow(dqfs$dqf)/2)))), t='n')
  click.points2 <- identify(cbind(seq(0,1,length.out=100), as.vector(t(dqfs$dqf / (nrow(dqfs$dqf)/2)))))
  click.points2 <- ceiling(click.points2/100)
  click.points <- c(click.points, click.points2)
  # round 2 - derivative of q-tilde
  plot(c(0,1), c(0,max(y.hat)), t='n', main="Select Observations - Press ESC when done", xlab="2 of 3", ylab="")
  for (i in 1:nrow(dqfs$dqf)) {
    lines(seq(0.0,1,length.out=99), y.hat[i,], lwd=.3, col=2)
  } 
  if (length(click.points)>0) {
    for (j in 1:length(click.points)) {
      i <- click.points[j] 
      lines(seq(0.0,1,length.out=99), y.hat[i,], lwd=1,  col=3)
    }  
  }
  points(cbind(seq(0,1,length.out=99),as.vector(t(y.hat))),t='n')
  click.points2 <- identify(cbind(seq(0,1,length.out=99), as.vector(t(y.hat))))
  click.points <- c(click.points, ceiling(click.points2/99))
  
  # round 3 q-tilde
  dqf.norm <- dqfs$dqf/apply(dqfs$dqf,1,max)
  plot(c(0,1), c(0,1), t='n', ylab="", main="Select Observations - Press ESC when done", xlab="3 of 3")
  for (i in 1:nrow(dqfs$dqf)) {
    lines(seq(0,1,length.out=n.qs), dqf.norm[i,], lwd=.3 + (sum(i==click.points)>0),  col=2+ (sum(i==click.points)>0))
  } 
  points(cbind(seq(0,1,length.out=100),as.vector(t(dqf.norm))), t='n')
  click.points2 <- identify(cbind(seq(0,1,length.out=100), as.vector(t(dqf.norm))))
  click.points2 <- ceiling(click.points2/100)
  click.points <- unique(c(click.points, click.points2))
  #finale
  par(mfrow=c(3,3))
  par(mar=c(0,0,0,0))
  
  plot.dqf(dqfs$dqf1, dqfs$angle[1], click.points)
  plot.dqf(dqfs$dqf2, dqfs$angle[2], click.points)
  plot.dqf(dqfs$dqf3, dqfs$angle[3], click.points)
  legend(.75,max(y.hat), click.points,1:length(click.points)+2, bty="n")
  return(click.points)
}