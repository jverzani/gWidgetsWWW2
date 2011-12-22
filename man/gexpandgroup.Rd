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

  \item{handler}{Called when expanded or closed}

  \item{action}{passed to handler}

  \item{horizontal}{left or right (default), or top to
  bottom (\code{horizontal=FALSE})}

  \item{spacing}{body spacing}

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

