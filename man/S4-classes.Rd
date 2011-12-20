\name{Observer-class}
\alias{BasicToolkitInterface-class}
\alias{Handler-class}
\alias{Observable-class}
\alias{Observer-class}
\title{Reference class to create an observer of an observable object}
\description{
  Reference class to create an observer of an observable
  object

  Handler is a special observer with obj and actino passed
  as first argument

  Observable class sets up objects that can be observed.
  Inherited by template

  This interface is inherited by the base GComponent
  classes in the toolkit implementations. The methods
  defined here are referenced by the S3 methods. For
  exampe, \code{svalue} dispatches to \code{get_value}.
}
\details{
  We combine both widget and container methods here. It
  isn't perfect, but they do share quite a bit. Perhaps, we
  could make the container class subclass the basic
  interface.
}

