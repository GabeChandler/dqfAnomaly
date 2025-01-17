% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dqfoutlier.R
\name{dqf.outlier}
\alias{dqf.outlier}
\title{DQF computation for data frame}
\usage{
dqf.outlier(
  data = NULL,
  gram.mat = NULL,
  g.scale = 2,
  angle = c(30, 45, 60),
  kernel = "linear",
  p1 = 1,
  p2 = 0,
  n.splits = 100,
  subsample = 50,
  z.scale = TRUE,
  k.w = 3,
  adaptive = TRUE,
  G = "norm"
)
}
\arguments{
\item{data}{data frame or matrix}

\item{gram.mat}{alternatively, the precompute Gram matrix}

\item{g.scale}{scales the base distribution G}

\item{angle}{3-vector of angles (must be between 0 and 90)}

\item{kernel}{of form "linear", "rbf" or "poly", or a user defined function}

\item{p1}{first parameter for kernel (if required)}

\item{p2}{second parameter for kernel (if required)}

\item{n.splits}{integer number of base points to compute DQF at}

\item{subsample}{integer number of random pairs to base averaged DQF on}

\item{z.scale}{logical to z-scale data prior to analysis}

\item{k.w}{integer number of points to windsorize at each extreme of data}

\item{adaptive}{logical to use adaptive DQF (windsorized st dev)}

\item{G}{base distribution:   "norm" or "unif"}

\item{angles}{of cone from midline, must live between 0 and 90}
}
\description{
Computes the averaged DQFs for a given data set
}
\examples{
fit.dqf <- dqf.outlier(iris[51:102,1:4], g.scale=6)
}
\keyword{dqf}
