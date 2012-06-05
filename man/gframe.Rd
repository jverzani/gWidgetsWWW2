\name{gframe}
\alias{gframe}
\title{gframe is a title-decorated ggroup box container}
\usage{
  gframe(text = "", pos = 0, horizontal = TRUE,
    spacing = 2, use.scrollwindow = FALSE,
    container = NULL, ..., width = NULL, height = NULL,
    ext.args = NULL)
}
\arguments{
  \item{text}{label text}

  \item{pos}{position of label. Ignored?}

  \item{horizontal}{left or right (default), or top to
  bottom (\code{horizontal=FALSE})}

  \item{spacing}{Margin around each child component in
  pixels. Can be a single number, in which case it is equal
  pixel space around each child. But for gWidgetsWWW2 one
  can specify a vector with recycling like function(top,
  right=top, bottom=top, left=right). A typical pattern is
  c(5,5,0,0), as otherwise there are 10 = 5 + 5 pixels
  between adjoing children. To get padding just around
  interior of box, pass in a value through ext.args, as in
  \code{ext.args=list(bodyPadding=10)}.}

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

  \item{...}{passed to \code{ggroup}}

  \item{width}{width in pixels of component. Sizing in
  \pkg{gWidgetsWWW2} is sometimes necessary as the
  arguments \code{expand} and \code{fill} are not well
  implemented.}

  \item{height}{height in pixels of the component.}

  \item{ext.args}{The contructors of \pkg{gWidgetsWWW2}
  ultimately call an Ext constructor. The options passed to
  the Ext constructor may be added to or overridden by use
  of this argument. Values are passed in as named list
  components and with values converted into JavaScript
  objects by \code{asJSObject}.}
}
\value{
  an \code{GContainer} object
}
\description{
  Use \code{svalue<-} to adjust the title
}
\examples{
w <- gwindow()
g <- gframe("Label", cont=w)
b <- gbutton("insider frame", cont=g)
svalue(g) <- "new label"
}
\seealso{
  \code{\link{ggroup}}
}

