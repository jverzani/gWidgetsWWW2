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

  \item{handler}{ignored}

  \item{action}{ignored}

  \item{container}{parent container}

  \item{...}{passed to parent container's \code{add}
  method}

  \item{width}{width in pixels}

  \item{height}{height in pixels}

  \item{ext.args}{extra arguments to pass to Ext
  constructor}
}
\value{
  an ExtWidget object
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

