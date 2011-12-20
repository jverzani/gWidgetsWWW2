\name{gcheckbox}
\alias{gcheckbox}
\title{checkbox widget}
\usage{
  gcheckbox(text = "", checked = FALSE, use.togglebutton =
  FALSE, handler = NULL, action = NULL, container = NULL,
  ..., width = NULL, height = NULL, ext.args = NULL)
}
\arguments{
  \item{text}{character. text label for checkbox.}

  \item{checked}{logical. initial state (Set later with
  \code{svalue<-})}

  \item{use.togglebutton}{logical. XXX not implemented If
  TRUE, represent with a togglebutton, else use check box}

  \item{handler}{handler called when state is toggled.
  Check value}

  \item{action}{action passed to handler}

  \item{container}{parent container}

  \item{...}{passed to \code{add} method of container.}

  \item{width}{width of widget. May be necessary, otherwise
  may take all horizontal real estate}

  \item{height}{height of widget (px)}

  \item{ext.args}{list of optional argument for ExtJS
  constructor}
}
\description{
  checkbox widget
}
\note{
  No method to set label
}
\examples{
w <- gwindow()
sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
cb <- gcheckbox("Check me?", cont=w, handler=function(h,...) if(svalue(h$obj)) galert("checked", parent=w))
}

