\name{gtext}
\alias{gtext}
\title{A text area widget}
\usage{
  gtext(text = NULL, width = NULL, height = 300, font.attr
  = NULL, wrap = TRUE, handler = NULL, action = NULL,
  container = NULL, ..., ext.args = NULL, resizable =
  FALSE)
}
\arguments{
  \item{text}{initial text}

  \item{width}{width in pixels}

  \item{height}{height in pixels}

  \item{font.attr}{Ignored. Default font attributes}

  \item{wrap}{Ignored Do we wrap the tet}

  \item{handler}{handler}

  \item{action}{action}

  \item{container}{parent container}

  \item{...}{passed to add method of parent container}

  \item{ext.args}{Extra arguments to pass to Ext
  constructor}

  \item{resizable}{Ignored. (Should area be resizable on
  the page)}
}
\value{
  an ExtWidget instance
}
\description{
  A text area widget
}
\examples{
w <- gwindow("gtext example")
sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
g <- ggroup(cont=w, horizontal=FALSE)
t <- gtext("Some text with \\n new lines", cont=g)
b <- gbutton("click", cont=g, handler=function(h,...) {
galert(svalue(b), parent=w)
})
b <- gbutton("change", cont=g, handler=function(h,...) {
svalue(t) <= "some new text"
})
}

