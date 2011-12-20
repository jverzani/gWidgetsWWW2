\name{gspinbutton}
\alias{gspinbutton}
\title{Basic spinbutton}
\usage{
  gspinbutton(from = 0, to = 100, by = 1, value = from,
  handler = NULL, action = NULL, container = NULL, ...,
  width = NULL, height = NULL, ext.args = NULL)
}
\arguments{
  \item{from}{from value}

  \item{to}{to}

  \item{by}{by. From to by are same as seq() usage}

  \item{value}{initial value}

  \item{handler}{handler for when change is made}

  \item{action}{passed to the handler, if given}

  \item{container}{parent container}

  \item{...}{passed to \code{add} method of container}

  \item{width}{width, ignored}

  \item{height}{height, ignored}

  \item{ext.args}{list. Can pass in other configuration
  arguments to Ext widget}
}
\value{
  an ExtWidget instance
}
\description{
  Basic spinbutton
}
\note{
  Buggy! For some reason both trigger icons don't show!
  Just the down arrow!
}
\examples{
w <- gwindow()
sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
sp <- gspinbutton(cont=w)
}

