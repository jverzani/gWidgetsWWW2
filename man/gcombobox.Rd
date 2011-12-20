\name{gcombobox}
\alias{gcombobox}
\title{combobox implementation}
\usage{
  gcombobox(items, selected = 1, editable = FALSE,
  coerce.with = NULL, handler = NULL, action = NULL,
  container = NULL, ..., width = NULL, height = NULL,
  ext.args = NULL, tpl = NULL)

  gdroplist(items, selected = 1, editable = FALSE,
  coerce.with = NULL, handler = NULL, action = NULL,
  container = NULL, ..., width = NULL, height = NULL,
  ext.args = NULL, tpl = NULL)
}
\arguments{
  \item{items}{a vector of items to choose from. (Not
  supported, but should be: Or a data frame with 1 column
  (items), two columns (items, icons), or three columns
  (items, icons, tooltip))}

  \item{selected}{initially selected item, by index. Use
  \code{0L} for none.}

  \item{editable}{logical. Does combobox allow editing. A
  bug (of package writer's limiations) in extjs do not
  allow one to set the value if it is a potential index. Go
  figure. Use 4.0, not 5 ...}

  \item{coerce.with}{Function. If given, called on value
  before returning}

  \item{handler}{handler}

  \item{action}{action}

  \item{container}{parent container}

  \item{...}{passed to \code{add} method of parent}

  \item{width}{width}

  \item{height}{height}

  \item{ext.args}{extra arguments to pass to constructor}

  \item{tpl}{a template for the item (Not working!)}
}
\value{
  an ExtWidget instance
}
\description{
  The \code{svalue<-} method is used to specify value by
  name or by index. The \code{[<-} method can be used to
  update the data to select from.

  The name gdroplist is an alternative (not encouraged) for
  gcomobbox
}
\note{
  The \code{tpl} argument is not working as we'd like.
}
\examples{
w <- gwindow()
sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
cb <- gcombobox(state.name, cont=w)
addHandlerChanged(cb, handler=function(h,...) {
galert(paste("You selected ", svalue(h$obj), sep=""), parent=w)
})
## type ahead style
cb$hide_trigger(TRUE)
}

