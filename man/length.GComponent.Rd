\name{length.GComponent}
\alias{length.GComponent}
\title{length method for GComponent's}
\usage{
  length(x, ...)
}
\arguments{
  \item{x}{object}

  \item{...}{passed along to \code{len} method}
}
\value{
  length of object, loosely interpreted
}
\description{
  This definition has some sideeffects. Namely, if one uses
  \code{sapply} and returns such objects, then the call to
  \code{simplify2array} will cause an error. Use
  \code{lapply}, or \code{simplify=FALSE}.
}

