\name{glabel}
\alias{glabel}
\title{A label widget}
\usage{
  glabel(text = "", markup = FALSE, editable = FALSE,
  handler = NULL, action = NULL, container = NULL, ...,
  width = NULL, height = NULL, ext.args = NULL)
}
\arguments{
  \item{text}{text for label. Main property. Use
  \code{svalue<-} to change.}

  \item{markup}{logical. Ignored, but see example for
  \code{ext.args} usage}

  \item{editable}{logical. Ignored}

  \item{handler}{ignored}

  \item{action}{ingnored}

  \item{container}{parent container}

  \item{...}{passed to parent container's \code{add}
  method}

  \item{width}{ignored}

  \item{height}{ignored}

  \item{ext.args}{additional options passed to Ext
  constructor.}
}
\value{
  an ExtWidget object
}
\description{
  A label widget
}
\examples{
w <- gwindow()
sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
glabel("A label widget", cont=w)
glabel("A red label", cont=w,  ext.args=list(style=list("color"="red")))
}

