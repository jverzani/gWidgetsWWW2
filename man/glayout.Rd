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

  \item{container}{A parent container. In
  \pkg{gWidgetsWWW2} a parent container is not optional
  (though it can be substituted with the \code{parent}
  argument in some circumstances). The parent specifies the
  widget heirarchy and the \code{...} argument is used to
  pass along arguments to layout the child component in the
  parent container. Typically, these are passed to the
  \code{add} method of the parent container.}

  \item{...}{Used to pass along argument to the parent
  container's ode{add} method and possible other arguments
  to the underlying ference class constructors.}

  \item{width}{width in pixels of component. Sizing in
  \pkg{gWidgetsWWW2} is sometimes necessary as the
  arguments \code{expand} and \code{fill} are not well
  implemented.}

  \item{height}{height in pixels of the component.}

  \item{ext.args}{The gWidgets' constructors ultimately
  call an Ext constructor. The options passed to the Ext
  constructor may be added to or overridden by use of this
  argument. Values are passed in a named list components
  and conversions to JavaScript objects}
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
  empty, one must be careful that no "holes" would be left
  over, as otherwise the layout will not work out
  correctly.
}
\details{
  The layout is only finalized after call the
  \code{visible<-} method with a value of \code{TRUE}. One
  adds all the desired children, then calls this method.
  This is a limitation of the underlying toolkit, Ext JS --
  children must be specified at configuration.
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
tbl[3,2] <- gbutton("click", cont=tbl, handler=function(h,...) {
galert(sprintf("\%s is a \%s", svalue(tbl[1,2]), svalue(tbl[2,2])), parent=w)
})
}

