\name{GContainer}
\alias{GContainer}
\title{Base reference class for container objects.}
\description{
  Container objects are those that hold other components
  and may be passed to constructors through their
  \code{container} argument. All objects keep a reference
  to their parent container, containers also keep
  references to their child components. See the
  \code{children} array, which is used for this.
}
\details{
  The main method for containers is the \code{add} method,
  though this is rarely called, as the underlying method is
  used in a widget's constructor when a parent container is
  specified through the standard \code{container} argument.
}

