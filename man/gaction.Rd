\name{gaction}
\alias{gaction}
\title{gaction implementation}
\usage{
  gaction(label, tooltip = label, icon = NULL, handler,
  parent, ...)
}
\arguments{
  \item{label}{Text for action}

  \item{tooltip}{tooltip. Ignored for this toolkit}

  \item{icon}{action icon class}

  \item{handler}{handler called when action activated}

  \item{parent}{toplevel window of action (where it can be
  called). Required here}

  \item{...}{ignored}
}
\value{
  an \code{ExtWidget} object
}
\description{
  actions are reusable encapsulations of actions. The
  \code{svalue<-} method can be used to update the text
  associated with an action. Use \code{enabled<-} to adjust
  sensitivity to mouse events. The \code{visible<-} method
  can adjust if objects proxying the action are visible.
  The \code{set_icon} reference class method can be used to
  set the icon (no S3 method)
}
\examples{
w <- gwindow()
a <- gaction("some action", handler=function(h,...) galert("asdf", parent=w), parent=w)
b <- gbutton(action=a, cont=w)
enabled(a) <- FALSE
svalue(a) <- "new text"
#
}

