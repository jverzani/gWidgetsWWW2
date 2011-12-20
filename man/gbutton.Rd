\name{gbutton}
\alias{gbutton}
\title{Basic button widget}
\usage{
  gbutton(text = "", handler = NULL, action = NULL,
  container = NULL, ..., width = NULL, height = NULL,
  ext.args = NULL)
}
\arguments{
  \item{text}{button text}

  \item{handler}{callback to call when button is clicked}

  \item{action}{passed to callback as \code{h$action}}

  \item{container}{parent container}

  \item{...}{passed to parent container's \code{add}
  method}

  \item{width}{width of button, if desired}

  \item{height}{height of button, if desired}

  \item{ext.args}{optional list of configuration options
  for ext widgets. For example
  \code{style=list(padding='20px')}.}
}
\value{
  a \code{GButton} instance
}
\description{
  A button responds to mouse clicks by calling its handler
}
\examples{
w  <- gwindow("test")
sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
g <- ggroup(cont=w, horizontal=FALSE)
b <- gbutton("click me", cont=g, handler=function(h,...) {
galert("hello world", parent=w)
})
## has an icon
b <- gbutton("up", cont=g)
## just an icon using ext override to remove text but leave icon
b <- gbutton("help", cont=g); b$call_Ext("setText")
## an action
a <- gaction("help", parent=w, handler=function(h,...) {
galert("action", parent=w)
})
b <- gbutton(action=a, cont=g)
}

