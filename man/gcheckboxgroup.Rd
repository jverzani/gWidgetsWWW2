\name{gcheckboxgroup}
\alias{gcheckboxgroup}
\title{A group of checkboxes}
\usage{
  gcheckboxgroup(items, checked = FALSE, horizontal =
  FALSE, use.table = FALSE, handler = NULL, action = NULL,
  container = NULL, ..., width = NULL, height = NULL,
  ext.args = NULL)
}
\arguments{
  \item{items}{vector of items to select from}

  \item{checked}{initial value of checked state. Recycled}

  \item{horizontal}{Layout horizontally?}

  \item{use.table}{Needs implementing. If TRUE, uses a grid
  widget with checkboxes to display. If TRUE, horizontal is
  ignored, and items may be a data frame.}

  \item{handler}{handler called when state changes}

  \item{action}{passed to handler}

  \item{container}{parent container}

  \item{...}{passed to add method of container}

  \item{width}{width of widget. May be necessary, otherwise
  may take all the horizontal real estate}

  \item{height}{height of widget (px)}

  \item{ext.args}{list of optional argument for ExtJS
  constructor}
}
\value{
  An ExtWidget instance
}
\description{
  A group of checkboxes
}
\examples{
w <- gwindow()
sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
cbg <- gcheckboxgroup(state.name[1:4], cont=w)
}

