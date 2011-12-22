\name{ggroup}
\alias{ggroup}
\title{Group, or box, container}
\usage{
  ggroup(horizontal = TRUE, spacing = 5, use.scrollwindow =
  FALSE, container, ..., width = NULL, height = NULL,
  ext.args = NULL)
}
\arguments{
  \item{horizontal}{left or right (default), or top to
  bottom (\code{horizontal=FALSE})}

  \item{spacing}{body spacing}

  \item{use.scrollwindow}{logical. If given, scrollbars
  will appear}

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
  a \code{GGroup} reference class object
}
\description{
  A box container. Can pack in left to right or top to
  bottom.
}
\examples{
w <- gwindow()
sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
g <- ggroup(cont=w, horizontal=FALSE) ## top to bottom
for(i in 1:10) gbutton(i, cont=g)
## add/delete
ctr <- 1 ### for label
addRow <- function(g) {
g1 <- ggroup(cont=g)
gbutton("x", cont=g1, handler=function(h,...) {delete(g, g1)})
glabel(paste("Click x to delete", ctr), cont=g1)
ctr <<- ctr + 1
}
g1 <- gframe("add/delete", cont=g, horizontal=FALSE)
addRow(g1)
f <- gframe("Adding and deleting", cont=g, horizontal=FALSE)
gbutton("+", cont=f, handler=function(h,...) addRow(f))
}

