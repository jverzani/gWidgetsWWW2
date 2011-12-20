\name{gradio}
\alias{gradio}
\title{Radio button group}
\usage{
  gradio(items, selected = 1, horizontal = FALSE, handler =
  NULL, action = NULL, container = NULL, ..., width = NULL,
  height = NULL, ext.args = NULL, columns =
  ifelse(horizontal, length(items), 1))
}
\arguments{
  \item{items}{Items to choose from}

  \item{selected}{index of initially selected item}

  \item{horizontal}{logical. Horizontal or vertical layout.
  (See also columns)}

  \item{handler}{called when radio buttons change}

  \item{action}{passed to handler}

  \item{container}{parent container}

  \item{...}{passed to add method of container}

  \item{width}{width in pixels}

  \item{height}{height in pixels}

  \item{ext.args}{list. Can pass in additional ext
  arguments, overriding defaults}

  \item{columns}{Can be used to override horizontal TRUE or
  FALSE}
}
\value{
  an ExtWidget object
}
\description{
  A basic radio button group. Specify the labels through
  items. The main value is the label or index
}
\note{
  the \code{[<-} method (to change the labels) is not
  implemented.
}
\examples{
w <- gwindow()
sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
rb <- gradio(state.name[1:4], cont=w)
addHandlerChanged(rb, handler=function(h,...) {
galert(paste("You clicked", svalue(h$obj)), parent=w)
})
svalue(rb, index=TRUE) <- 1 # by index
svalue(rb) <- state.name[2] # by value
## rb[] <- state.name[1:6] ## doesn't work!
}

