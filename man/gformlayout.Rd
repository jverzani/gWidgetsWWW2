\name{gformlayout}
\alias{gformlayout}
\title{A form layout container}
\usage{
  gformlayout(align = c("default", "left", "center", "right", "top"),
    spacing = 5, container = NULL, ..., width = NULL,
    height = NULL, ext.args = NULL)
}
\arguments{
  \item{align}{alignment of labels, from \code{c("default",
  "left", "center", "right", "top")}.}

  \item{spacing}{spacing between columns}

  \item{container}{parent container}

  \item{...}{passed to \code{add} method of parent
  container.}
}
\description{
  A simple way to layout forms with labels.  The label is
  passed to the \code{label} argument of the control's
  constructor which is then passed along to the \code{add}
  method of the parent container.
}
\examples{
\dontrun{
w <- gwindow("gformlayout")
g <- gvbox(container=w)

flyt <- gformlayout(container=g)
gedit("", label="Name:", container=flyt)
gedit("", label="Rank:", container=flyt)
gedit("", label="Serial No.:", container=flyt)

b <- gbutton("Show me", container=g, handler=function(h,...) {
print(svalue(flyt))
})

}
}

