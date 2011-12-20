\name{gstatusbar}
\alias{gstatusbar}
\title{Status bar for gwindow instances}
\usage{
  gstatusbar(text = "", container = NULL, ...)
}
\arguments{
  \item{text}{text for label}

  \item{container}{parent window}

  \item{...}{ignored}
}
\value{
  an ExtWidget instance
}
\description{
  Status for main window. Use \code{gwindow} instance for
  parent container. The \code{svalue<-} method can be used
  to change the value.
}
\examples{
w <- gwindow()
sb <- gstatusbar("Powered by gWidgetsWWW and Rook")
}

