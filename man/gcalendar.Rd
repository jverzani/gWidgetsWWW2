\name{gcalendar}
\alias{gcalendar}
\title{calendar widget}
\usage{
  gcalendar(text = "", format = NULL, handler = NULL,
  action = NULL, container = NULL, ..., width = NULL,
  height = NULL, ext.args = NULL)
}
\arguments{
  \item{text}{optional inital date as text.}

  \item{format}{format of date. Default of Y-m-d.}

  \item{handler}{handler called when date changed}

  \item{action}{action passed to handler}

  \item{container}{parent container}

  \item{...}{passed to \code{add} method of container}

  \item{width}{width of widget in pixels}

  \item{height}{height of widget in pixels}

  \item{ext.args}{extra arguments passed to Ext
  constructor}
}
\value{
  a \code{ExtWidget} instance
}
\description{
  Basic text box with button to open calendar picker
  dialog. The svalue method refers to the date, which
  depends on the value of format.xf
}
\examples{
w <- gwindow("Calendar")
sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
a <- gcalendar(cont=w)
}

