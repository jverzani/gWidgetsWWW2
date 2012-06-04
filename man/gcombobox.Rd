\name{gcombobox}
\alias{gcombobox}
\alias{gdroplist}
\title{combobox implementation}
\usage{
  gcombobox(items, selected = 1, editable = FALSE,
    coerce.with = NULL, handler = NULL, action = NULL,
    container = NULL, ..., width = NULL, height = NULL,
    ext.args = NULL, autocomplete = FALSE,
    initial.msg = "", tpl = NULL)

  gdroplist(items, selected = 1, editable = FALSE,
    coerce.with = NULL, handler = NULL, action = NULL,
    container = NULL, ..., width = NULL, height = NULL,
    ext.args = NULL, autocomplete = FALSE,
    initial.msg = "", tpl = NULL)
}
\arguments{
  \item{items}{a vector of items to choose from. Coerced to
  character class. Use \code{coerce.with} to get a numeric
  value, say. (Not supported, but should also allow: Or a
  data frame with 1 column (items), two columns (items,
  icons), or three columns (items, icons, tooltip))}

  \item{selected}{initially selected item, by index. Use
  \code{0L} for none.}

  \item{editable}{logical. Does combobox allow editing. A
  bug (or package writer's limiations) in extjs do not
  allow one to set the value if it is a potential index
  (when \code{items} is integer). Go figure. Use 4.0, not 5
  if you want numeric values ...}

  \item{coerce.with}{Function. If given, called on value
  before returning}

  \item{autocomplete}{If \code{TRUE}, will hide the trigger
  and make editable. When the user types the matching
  values are presented. (This is also the default
  behaviour, but if the object is not editable, only valid
  values are stored.)}

  \item{initial.msg}{If \code{selected=0}, then one can
  assign an initial message here}

  \item{tpl}{a template for the item (Not working!
  Dang....)}

  \item{handler}{optional means (to
  \code{\link{addHandlerChanged}}) to specify a handler for
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
  container's \code{add} method and possible other
  arguments to the underlying ference class constructors.}

  \item{width}{width in pixels of component. Sizing in
  \pkg{gWidgetsWWW2} is sometimes necessary as the
  arguments \code{expand} and \code{fill} are not well
  implemented.}

  \item{height}{height in pixels of the component.}

  \item{ext.args}{The contructors of \pkg{gWidgetsWWW2}
  ultimately call an Ext constructor. The options passed to
  the Ext constructor may be added to or overridden by use
  of this argument. Values are passed in as named list
  components and with values converted into JavaScript
  objects by \code{\link{asJSObject}}.}
}
\value{
  an ExtWidget instance
}
\description{
  The \code{svalue<-} method is used to specify value by
  name or by index.  (THIS IS BROKEN, LIKELY A BUG IN EXTJS
  CODE, BUT [<- DOES NOT WORK The \code{[<-} method can be
  used to update the data to select from.)

  The name gdroplist is an alternative (not encouraged) for
  gcomobbox
}
\details{
  The default change handler differs depending whether the
  field is editable. If not, then the handler is called
  before the selection is finalized. Otherwise, the handler
  is called on the "change" event, which is to track
  "user-initiated change is detected in the value of the
  field." This does not always fire on selection by an
  item. To force that, use the \code{add_handler_select}
  method.
}
\note{
  The \code{[<-} method is note working. The \code{tpl}
  argument is not working as we'd like.
}
\examples{
w <- gwindow()
sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
g <- ggroup(cont=w, horizontal=FALSE)
cb <- gcombobox(state.name, cont=g)
addHandlerChanged(cb, handler=function(h,...) {
galert(paste("You selected ", svalue(h$obj), sep=""), parent=w)
})
## initial message
gcombobox(state.name, cont=g, selected=0, initial.msg="Choose a state...")
## autocomplete
 cb <- gcombobox(state.name, cont=g, autocomplete=TRUE)
}

