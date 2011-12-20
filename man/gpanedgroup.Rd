\name{gpanedgroup}
\alias{gpanedgroup}
\title{A paned group container}
\usage{
  gpanedgroup(horizontal = TRUE, container = NULL, ...,
  width = NULL, height = 400, ext.args = NULL, collapsible
  = FALSE, default.size = NULL)
}
\arguments{
  \item{horizontal}{logical. Side by side (or top/bottom)
  layout}

  \item{container}{parent container}

  \item{...}{passed to parent container's add method}

  \item{width}{width of container in pixels}

  \item{height}{height of container in pixels}

  \item{ext.args}{list. Passed to Ext constructor}

  \item{collapsible}{logical. If TRUE, one child wil have a
  button so that it can be collapsed}

  \item{default.size}{Integer pixel size of right or bottom
  container when drawn. Defaults to half of height or
  width, or 200 if those are NULL. There is no
  \code{svalue<-} method to adjust the size}
}
\value{
  an ExtContainer object
}
\description{
  This container has two children and a vertical/horizontal
  sash to allocate space between them. Simply use as a
  parent container twice. Unlike other gWidgets
  implementations, \code{svalue<-} is not implemented.
}
\examples{
w <- gwindow()
pg <- gpanedgroup(cont=w, horizontal=TRUE)
gbutton("left", cont=pg)
gbutton("right", cont=pg)
}

