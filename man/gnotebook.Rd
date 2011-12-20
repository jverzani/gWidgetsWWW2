\name{gnotebook}
\alias{gnotebook}
\title{Notebook container}
\usage{
  gnotebook(tab.pos = 3, close.buttons = FALSE, container,
  ..., width = NULL, height = NULL, ext.args = NULL)
}
\arguments{
  \item{tab.pos}{where to place tabs. A value of 1 is the
  bottom, else the top.}

  \item{close.buttons}{Logical. Are there close buttons on
  the tabs}

  \item{container}{parent container}

  \item{...}{passed to add method of parent container}

  \item{width}{width in pixels, if specified}

  \item{height}{height in pixels, if specified}

  \item{ext.args}{extra arguments to pass to Ext
  constructor}
}
\value{
  an ExtContainer object
}
\description{
  Notebook container
}
\examples{
w <- gwindow()
nb <- gnotebook(cont=w)
gbutton("hello", container=nb, label="My label") ## pass in label argument through ... to \\code{add}
}

