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

  \item{container}{A parent container. In
  \pkg{gWidgetsWWW2} a parent container is not optional
  (though it can be substituted with the \code{parent}
  argument in some circumstances). The parent specifies the
  widget heirarchy and the \code{...} argument is used to
  pass along arguments to layout the child component in the
  parent container. Typically, these are passed to the
  \code{add} method of the parent container.}

  \item{...}{Used to pass along argument to the parent
  container's \code{add} method and possible other
  arguments to the underlying ference class constructors.}

  \item{width}{width in pixels of component. Sizing in
  \pkg{gWidgetsWWW2} is sometimes necessary as the
  arguments \code{expand} and \code{fill} are not well
  implemented.}

  \item{height}{height in pixels of the component.}

  \item{ext.args}{The contructors of \pkg{gWidgetsWWW2}
  ultimately call an Ext constructor. The options passed to
  the Ext constructor may be added to or overridden by use
  of this argument. Values are passed in as named list
  components and with values converted into JavaScript
  objects by \code{\link{asJSObject}}.}
}
\value{
  a \code{GDf} reference class object.
}
\description{
  A widget for editing a data frame. There is no means to
  change the size of the frame being edited or the type of
  data in each column. As such, one must plan ahead. This
  implementation allows one to add a row to the data store,
  to edit a row (row-by-row), but there is no means to
  delete a row.
}
\author{
  john verzani
}

