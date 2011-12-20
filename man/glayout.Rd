\name{glayout}
\alias{glayout}
\title{A table container}
\usage{
  glayout(homogeneous = FALSE, spacing = 2, container =
  NULL, ..., width = NULL, height = NULL, ext.args = NULL)
}
\arguments{
  \item{homogeneous}{equal sized columns/rows?}

  \item{spacing}{between cell spacing}

  \item{container}{parent container}

  \item{...}{passed to add method of parent container}

  \item{width}{width}

  \item{height}{height}

  \item{ext.args}{extra arguments for Ext constructor}
}
\value{
  an ExtContainer object
}
\description{
  A table container allows one to organize child widgets in
  a grid using matrix notation to indicate where the child
  should be placed. The basic assignment is: \code{tbl[i,j]
  <- gbutton("asdf", cont=tbl)}. The \code{tbl} object is
  on the right side, so the widget has a toplevel and on
  the left to specify how the layout will occur. Although
  one can specify i and j as a range of values, or even
  empty one must be careful that no "holes" would be left
  over, as otherwise the layout will not work out
  correctly.
}
\details{
  The layout is only finalized after call the
  \code{visible<-} method with a value of \code{TRUE}. One
  adds all the desired children, then calls this method.
}
\examples{
w <- gwindow("grid layout example")
sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
tbl <- glayout(cont=w)
tbl[1,1, anchor=c(1,0)] <- "Name"  ## a character maps to a label
tbl[1,2] <- gedit("", cont=tbl) ## tbl needed on right side too
tbl[2,1, anchor=c(1,0)] <- "Rank"
tbl[2,2] <- gedit("", cont=tbl)
visible(tbl) <- TRUE ## This line is needed!
}

