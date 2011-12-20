\name{gfile}
\alias{gfile}
\title{File selection form}
\usage{
  gfile(text = "Choose a file", type = c("open"), filter =
  NULL, handler = NULL, action = NULL, container = NULL,
  ..., width = NULL, height = NULL, ext.args = NULL)
}
\arguments{
  \item{text}{Instructional text.}

  \item{type}{only "open" implemented}

  \item{filter}{ignored}

  \item{handler}{Called when file is succesfully uploaded}

  \item{action}{passed to handler when callled}

  \item{container}{parent container}

  \item{...}{passed to add method of parent container}

  \item{width}{width in pixels}

  \item{height}{ignored}

  \item{ext.args}{extra args to pass to constructor}
}
\value{
  an ExtWidget
}
\description{
  This renders a file selection form within a small panel
  widget. There are two steps: select a file, then save the
  file to the server.
}
\note{
  the \code{svalue} method returns the temporary filename
  of the uploaded file, or a value of \code{NA}. The
  property \code{filename} holds the original filename.
}
\examples{
w <- gwindow()
gstatusbar("Powered by gWidgetsWWW2 and Rook", cont=w)
f <- gfile("Choose a file for upload", cont=w, handler=function(h,...) {
galert(paste("You uploaded", svalue(h$obj)), parent=w)
})
}

