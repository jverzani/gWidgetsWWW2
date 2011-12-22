\name{gimage}
\alias{gimage}
\title{Container for an image}
\usage{
  gimage(filename = "", dirname = "", size = "", handler =
  NULL, action = NULL, container = NULL, ..., width = NULL,
  height = NULL, ext.args = NULL)
}
\arguments{
  \item{filename}{A url or file in static temp file}

  \item{dirname}{prepended to filename if non empty}

  \item{size}{passed to \code{size<-} method if given}

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
  an GImage reference object
}
\description{
  The image shows a url. This url can be external. For
  local urls, the file must be accessible by the server.
  The convenience method \code{obj$get_tempfile()} or the
  function \code{get_tempfile} can be used to create a file
  that is accessible. See the example.
}
\note{
  requires tempdir to be mapped to a specific url, as this
  is assumed by \code{get_tempfile} and
  \code{get_tempfile_url}
}
\examples{
w <- gwindow("hello", renderTo="replaceme")
sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
g <- ggroup(cont=w, horizontal=FALSE)

f <- get_tempfile(ext=".png")
png(f)
hist(rnorm(100))
dev.off()

i <- gimage(f, container=g)
b <- gbutton("click", cont=g, handler=function(h,...) {
f <- i$get_tempfile(ext=".png")   ## a method call, for variety
png(f)
hist(rnorm(100))
dev.off()
svalue(i) <- f
})
}

