\name{gtable}
\alias{gtable}
\title{A table widget}
\usage{
  gtable(items, multiple = FALSE, chosencol = 1, icon.FUN =
  NULL, filter.column = NULL, filter.labels = NULL,
  filter.FUN = NULL, handler = NULL, action = NULL,
  container = NULL, ..., width = 200, height = 200,
  ext.args = NULL, paging = FALSE, col.widths = rep(20,
  ncol(as.data.frame(items))))
}
\arguments{
  \item{items}{data frame of items to display}

  \item{multiple}{logical. TRUE for multiple selections}

  \item{chosencol}{The svalue() method returns a single
  value, by default. This species column of that value.}

  \item{icon.FUN}{NOT IMPLEMENTED. Instead, just add a
  column with class Icon containing css class of the icons}

  \item{filter.column}{Ignored}

  \item{filter.labels}{Ignored}

  \item{filter.FUN}{Ignored.}

  \item{handler}{single click handlers}

  \item{action}{action passed to handler}

  \item{container}{parent container}

  \item{...}{passed to parent container's \code{add}
  method}

  \item{width}{width in pixels}

  \item{height}{height in pixels, should be set, otherwise
  get only 1 row}

  \item{ext.args}{additional configuration values to pass
  to constructor}

  \item{paging}{Either a logical variable or integer. If
  \code{TRUE} then paging will be used which allows only
  chunks of the data to be sent to the browser at a time
  (default size = 200 rows). If \code{integer} then paging
  is turned on and this value overrides the default page
  size.}

  \item{col.widths}{width of each column. Also see
  \code{size<-} with a list where \code{columnWidths} is
  specified.}
}
\value{
  An ExtWidget instance
}
\description{
  A widget for displaying a data frame in tabular format.
  The main property is the set of indices for the currently
  selected rows. For large data sets, the data can be
  "paged", that is given to the browser in bite-sized
  chunks so the lag is lacking.  The change handler is for
  a single click, also used for selection. Use
  \code{addHandlerDoublecclick} to specify a callback for
  the double click event on a cell.
}
\details{
  The column names are inherited from the columnnames of
  the data frame. Names with spaces will render, but are
  not sortable. Use some punctuation, such as an underscore
  to get sortability.

  A column of class "Icon" (see \code{\link{asIcon}}) will
  render a css class as an icon. See the example.

  The item replacement operator \code{[<-} will work only
  if all the column types remain constant, as the column
  renderers are set at time of construction. This also
  effects the initial data frame. Pass a 0-row data frame
  with column names and defined column types at
  construction if no data is known at that point.
}
\note{
  With \code{width} and/or \code{height} set to
  \code{NULL}, the default size will likely be
  unsatisfying. These values are best set by the
  programmer. They can be readjusted through the
  \code{size<-} method. For \pkg{gWidgetsWWW}, the
  \code{filter} functions are not implemented. Rather the
  \code{filter} method may be used to filter a named column
  by a regular expression. See the example.
}
\examples{
w <- gwindow("Filtering and table example")
sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
g <- ggroup(cont=w, horizontal=FALSE)
g1 <- ggroup(cont=g)
glabel("Filter by:", cont=g1)
e <- gedit("", cont=g1)
tbl <- gtable(data.frame(name=state.name, stringsAsFactors=FALSE), cont=g,  multiple=TRUE)
addHandlerKeystroke(e, handler=function(h,...) {
val <- svalue(h$obj)
if(nchar(val) > 0) {
tbl$filter("name", val)
}
})
b <- gbutton("click", cont=g, handler=function(h,...) galert(svalue(tbl, index=FALSE), parent=w))

## icons
m <- mtcars[1:3, 1:4]
## add icons as css class
m[,1] <- asIcon(c("up","up","down"))
gtable(m, cont=g)
}

