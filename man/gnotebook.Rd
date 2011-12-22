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

  \item{container}{A parent container. In
  \pkg{gWidgetsWWW2} a parent container is not optional
  (though it can be substituted with the \code{parent}
  argument in some circumstances). The parent specifies the
  widget heirarchy and the \code{...} argument is used to
  pass along arguments to layout the child component in the
  parent container. Typically, these are passed to the
  \code{add} method of the parent container.}

  \item{...}{Used to pass along argument to the parent
  container's ode{add} method and possible other arguments
  to the underlying ference class constructors.}

  \item{width}{width in pixels of component. Sizing in
  \pkg{gWidgetsWWW2} is sometimes necessary as the
  arguments \code{expand} and \code{fill} are not well
  implemented.}

  \item{height}{height in pixels of the component.}

  \item{ext.args}{The gWidgets' constructors ultimately
  call an Ext constructor. The options passed to the Ext
  constructor may be added to or overridden by use of this
  argument. Values are passed in a named list components
  and conversions to JavaScript objects}
}
\value{
  a GNotebook reference class object.
}
\description{
  Notebook container
}
\examples{
w <- gwindow()
nb <- gnotebook(cont=w)
gbutton("hello", container=nb, label="My label") ## pass in label argument through ... to \\code{add}
}

