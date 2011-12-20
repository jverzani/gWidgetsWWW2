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

  \item{handler}{Change handler. Change is a "blur" event
  (when widget loses focus) and when key is activated.}

  \item{action}{passed to handler}

  \item{container}{parent container}

  \item{...}{passed to add method of parent container}

  \item{ext.args}{extra arguments to pass to constructor}
}
\value{
  an ExtWidget object
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

