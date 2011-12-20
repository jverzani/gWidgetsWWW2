\name{gdf}
\alias{gdf}
\title{A widget for editing a data frame}
\usage{
  gdf(items = NULL, name = deparse(substitute(items)),
  do.subset = FALSE, container = NULL, ..., width = 300,
  height = 200, ext.args = NULL)
}
\arguments{
  \item{items}{data frame to be edited}

  \item{name}{name of data frame appearing in titke}

  \item{do.subset}{Ignored.}

  \item{container}{Parent container}

  \item{...}{passed to container's \code{add} method}

  \item{width}{width of widget in pixels}

  \item{height}{height of widget in pixels}

  \item{ext.args}{extra arguments to pass to constructor}
}
\value{
  an ExtWidget
}
\description{
  A widget for editing a data frame. There is no means to
  change the size of the frame being edited or the type of
  data in each column. As such, one must plan ahead.
}
\author{
  john verzani
}

