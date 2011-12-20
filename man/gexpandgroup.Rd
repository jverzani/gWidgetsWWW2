\name{gexpandgroup}
\alias{gexpandgroup}
\title{gexpandgroup is a group with trigger icon and label}
\usage{
  gexpandgroup(text = "", horizontal = TRUE, spacing = 5,
  handler = NULL, action = NULL, container = NULL, ...,
  width = NULL, height = NULL, ext.args = NULL)
}
\arguments{
  \item{text}{label text}

  \item{horizontal}{logical. Indicates direction children
  are added}

  \item{spacing}{between component spacing}

  \item{handler}{Called when expanded or closed}

  \item{action}{passed to handler}

  \item{container}{parent container}

  \item{...}{passed to add method of parent}

  \item{width}{width in pixels}

  \item{height}{height in pixels}

  \item{ext.args}{passed to constructor}
}
\description{
  Use \code{svalue<-} to adjust the title. The
  \code{visible<-} method is used to programatically change
  display
}
\examples{
w <- gwindow()
sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
g <- gexpandgroup("click to close", cont=w)
gbutton("some button", cont=g)
visible(g) <- FALSE ## to close
}
\seealso{
  \code{\link{ggroup}}, \code{\link{gframe}}
}

