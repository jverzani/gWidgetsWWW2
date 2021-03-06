% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/gtext.R
\name{gtext}
\alias{gtext}
\title{A text area widget}
\usage{
gtext(text = NULL, width = NULL, height = 300, font.attr = NULL,
  wrap = TRUE, handler = NULL, action = NULL, container = NULL, ...,
  ext.args = NULL)
}
\arguments{
\item{text}{initial text}

\item{width}{width in pixels}

\item{height}{height in pixels}

\item{font.attr}{Ignored. Default font attributes}

\item{wrap}{Ignored Do we wrap the tet}

\item{handler}{optional means (to \code{\link{addHandlerChanged}})
to specify a handler for the default signal. A handler is a
function with signature \code{(h,...)} where \code{h} is a list
with components \code{obj} referring to the object emitting the
signal, \code{action} containing values passed to the
\code{action} argument, and possible other values.}

\item{action}{Passed to handler to parameterize a call}

\item{container}{A parent container. In \pkg{gWidgetsWWW2} a
parent container is not optional (though it can be substituted
with the \code{parent} argument in some circumstances). The parent
specifies the widget heirarchy and the \code{...} argument is used
to pass along arguments to layout the child component in the
parent container. Typically, these are passed to the \code{add}
method of the parent container.}

\item{...}{Used to pass along argument to the parent container's
\code{add} method and possible other arguments to the underlying
ference class constructors.}

\item{ext.args}{The contructors of \pkg{gWidgetsWWW2} ultimately
call an Ext constructor. The options passed to the Ext constructor
may be added to or overridden by use of this argument. Values are
passed in as named list components and with values converted into JavaScript
objects by \code{asJSObject}.}
}
\value{
an ExtWidget instance
}
\description{
A text area widget
}
\examples{
w <- gwindow("gtext example")
sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
g <- ggroup(cont=w, horizontal=FALSE)
t <- gtext("Some text with \\n new lines", cont=g)
b <- gbutton("click", cont=g, handler=function(h,...) {
  galert(svalue(b), parent=w)
})
 b <- gbutton("change", cont=g, handler=function(h,...) {
   svalue(t) <= "some new text"
})
}

