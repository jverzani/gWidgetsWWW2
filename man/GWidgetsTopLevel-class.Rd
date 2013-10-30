\name{GWidgetsTopLevel-class}
\alias{GWidgetsTopLevel-class}
\title{A toplevel object for a gWidgetsWWW2 app stores page-specific values}
\usage{
  GWidgetsTopLevel(...)
}
\description{
  The toplevel object for gWidgetsWWW2 apps stores page
  specific information. As there can potentially be several
  \code{gwindow} instances on a page (using
  \code{renderTo}), however all share the same toplevel
  instance. This instance holds a reference to the
  evaluation environment, the request that generates the
  page, the objects populating the page, and a queue for
  holding JavaScript commands (produced by method calls,
  say).
}
\details{
  The sharing of this toplevel object amongs each component
  is the reason why the constructors require a
  specification of either a \code{container} or
  \code{parent} argument.
}

