\name{gcodemirror}
\alias{gcodemirror}
\title{codemirror}
\usage{
  gcodemirror(text = "", container = NULL, ..., width =
  NULL, height = NULL, ext.args = list())
}
\arguments{
  \item{text}{initial text.}

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
  a GCodeMirror reference class
}
\description{
  A gtext object modified to use codemirror for syntax
  highlighting
}
\details{
  The CodeMirror project (codemirror.net) provides a code
  editor in javascript with an R mode. This widget
  incorporates that into \pkg{gWidgetsWWW2}.
}
\author{
  john verzani
}

