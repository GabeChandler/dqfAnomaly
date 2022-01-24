#' Visualizes DQFs computed from dqf.outlier function
#'
#' Dependency function for dqf.explore
#' @param dqf a matrix of dqfs
#' @param angle angle used in computation of dqf
#' @param click.points vector of points to highlight
#' @keywords dqf
#' @export 

plot.dqf <- function(dqf, angle, click.points) {
  ## Dependency for dqf.explore
  ## Creates 3 versions of DQF information
  n.qs <- ncol(dqf)
  y.hat <- matrix(0, nrow=nrow(dqf), ncol=99)
  for (i in 1:nrow(dqf)) {
    y <- dqf[i,]/ max(dqf[i,])
    x <- seq(0,1,length.out=length(y))
    y.hat[i,] <- diff(ksmooth(x,y,kernel="normal", band=.05, n.points=100)$y)
  }
  dqf.norm <- dqf/apply(dqf,1,max)
  plot(c(0,1), c(0,1), t='n', ylab="", xaxt="n", yaxt="n")
  for (i in 1:nrow(dqf)) {
    lines(seq(0,1,length.out=n.qs), dqf.norm[i,], lwd=.3,  col=2)
  } 
  if (length(click.points)>0) {
    for (j in 1:length(click.points)) {
      i <- click.points[j] 
      lines(seq(0,1,length.out=n.qs), dqf.norm[i,], lwd=1,  col=2+j)
    }  
  }
  legend(.8,.3,angle, bty="n")
  plot(c(0,1), c(0,max(dqf/(nrow(dqf)/2) )), t='n', ylab="", xaxt="n", yaxt="n")
  for (i in 1:nrow(dqf)) {
    lines(seq(0,1,length.out=n.qs), dqf[i,]/ (nrow(dqf)/2), lwd=.3, col=2)
  } 
  if (length(click.points)>0) {
    for (j in 1:length(click.points)) {
      i <- click.points[j] 
      lines(seq(0,1,length.out=n.qs), dqf[i,]/ (nrow(dqf)/2), lwd=1,  col=2+j)
    }  
  }
  plot(c(0,1), c(0,max(y.hat)), t='n', ylab="", xaxt="n", yaxt="n")
  for (i in 1:nrow(dqf)) {
    lines(seq(0.0,1,length.out=99), y.hat[i,], lwd=.3, col=2)
  }
  if (length(click.points)>0) {
    for (j in 1:length(click.points)) {
      i <- click.points[j] 
      lines(seq(0,1,length.out=99), y.hat[i,], lwd=1,  col=2+j)
    } 
  }
}