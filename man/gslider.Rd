\name{gslider}
\alias{gslider}
\title{slider widget}
\usage{
  gslider(from = 0, to = 100, by = 1, value = from,
  horizontal = TRUE, handler = NULL, action = NULL,
  container = NULL, ..., width = NULL, height = NULL,
  ext.args = NULL, tpl = "{0}")
}
\arguments{
  \item{from}{starting point. Unlike other implementations
  for gWidgets, this is not possibly a vector specifying a
  range of values to slide over.}

  \item{to}{ending point}

  \item{by}{step size. Must be larger than 1 and even then
  will round to integer value}

  \item{value}{initial value}

  \item{horizontal}{orientation}

  \item{tpl}{Template for tooltip. Should have "\code{{0}}"
  to replace the value, but can have more formatting}

  \item{handler}{optional means (to
  \code{\link{addHandlerChanged}} to specify a handler for
  the default signal. A handler is a function with
  signature \code{(h,...)} where \code{h} is a list with
  components \code{obj} referring to the object emitting
  the signal, \code{action} containing values passed to the
  \code{action} argument, and possible other values.}

  \item{action}{Passed to handler to parameterize a call}

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
  a \code{GSlider} reference class object
}
\description{
  Use slider to select from a sequence of values, specified
  as with \code{seq}. The sequence steps must be greater
  than 1
}
\examples{
w <- gwindow()
sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
sl <- gslider(from=0, to=100, by=1, value=50, cont=w, tpl="{0}percent")
addHandlerChanged(sl, handler=function(h,...) {
galert(paste("You selected", svalue(h$obj)), parent=w)
})
}

