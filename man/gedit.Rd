\name{gedit}
\alias{gedit}
\title{gedit widget}
\usage{
  gedit(text = "", width = 25, coerce.with = NULL,
  initial.msg = "", handler = NULL, action = NULL,
  container = NULL, ..., ext.args = NULL)
}
\arguments{
  \item{text}{initial text}

  \item{width}{width in characters. Converted to pixels by
  multiplying by 8.}

  \item{coerce.with}{Function to call for coercion from
  text. If no coercion be careful when using the values, as
  the user can potentiall type in malicious things.}

  \item{initial.msg}{initial message to user, in greyed out
  text}

  \item{handler}{optional means (to
  \code{\link{addHandlerChanged}} to specify a handler for
  the default signal. A handler is a function with
  signature \code{(h,...)} where \code{h} is a list with
  components \code{obj} referring to the object emitting
  the signal, \code{action} containing values passed to the
  \code{action} argument, and possible other values.}

  \item{action}{Passed to handler to parameterize a call}

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

  \item{ext.args}{The gWidgets' constructors ultimately
  call an Ext constructor. The options passed to the Ext
  constructor may be added to or overridden by use of this
  argument. Values are passed in a named list components
  and conversions to JavaScript objects}
}
\value{
  a \code{GEdit} referece class object
}
\description{
  No [<- method. This can be done with a combobox though.
}
\examples{
w <- gwindow()
sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
e <- gedit("initial text", cont=w)
addHandlerChanged(e, handler=function(h,...) {
galert(paste("You entered", svalue(h$obj)), parent=w)
})
}

