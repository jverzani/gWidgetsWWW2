\name{gborderlayout}
\alias{gborderlayout}
\title{A "border" layout is a 5-panel layout where "north", "east",
"south", and "west" satellite panels surround a "center" panel.}
\usage{
  gborderlayout(container = NULL, ..., width = NULL, height
  = NULL, ext.args = NULL, title = list(), collapsible =
  list())
}
\arguments{
  \item{title}{a list with named components from
  \code{c("center","north", "south", "east", "west"))}
  allowing one to specify titles (as length-1 character
  vectors) of the regions. The default is no title. A title
  may be added later by adding a \code{gframe} instance,
  but that won't work well with a collapsible panel.}

  \item{collapsible}{a list with named components from
  \code{c("center","north", "south", "east", "west"))}
  allowing one to specify through a logical if the panel
  will be collapsible, similar to \code{gexpandgroup}. The
  default is \code{FALSE}}
}
\value{
  a \code{GBorderLayout} reference class object
}
\description{
  The \code{gborderlayout} container implements a border
  layout where 4 panels surround a main center panel. The
  panels may be configured with a title (like
  \code{gframe}) and may be collpasible (like
  \code{gexpandgroup}). Both configurations are done at
  construction time. The panels only hold one child, so one
  would add a container to have more complicated layouts.
}
\details{
  To add a child component, one specifies a value of
  \code{where} to the \code{add} method (implicitly called
  by the constructor, so in practice this argument is
  passed through \code{...} by the constructor). The value
  of \code{where} if from \code{c("center","north",
  "south", "east", "west")}. Child components are added
  with the "fit" layout, which is basically the same as
  specifying \code{expand=TRUE} and \code{fill=TRUE},
  though those arguments are ignored here.

  The satellite panels may be resized through the reference
  class method \code{set_panel_size} with arguments
  \code{where} and a numeric \code{dimension}.
}
\examples{
w <- gwindow("dialogs")
#
bl <- gborderlayout(cont=w,
title=list(center="State facts (state.x77)", west="Select a state"),
collapsible=list(west=TRUE)
)
#
tbl <- gtable(data.frame(states=rownames(state.x77), stringsAsFactors=FALSE),
multiple=FALSE,
cont=bl, where="west")
size(tbl) <- c("100\%", "100\%")
bl$set_panel_size("west", 200)
#
g <- ggroup(cont=bl, where="center", horizontal=FALSE, height=500, width=500)
nms <- colnames(state.x77)

labs <- lapply(seq_along(nms), function(i) {
g1 <- ggroup(cont=g, width=500)
glabel(sprintf("<b>\%s</b>",nms[i]), cont=g1)
glabel("", cont=g1)
})
#
update_state_info=function(h,...) {
nm <- svalue(h$obj)
nm <- sample(rownames(state.x77),1)
facts <- state.x77[nm,]
sapply(seq_along(facts), function(i) {
lab <- labs[[i]]
svalue(lab) <- facts[i]
})
}
addHandlerChanged(tbl, handler=update_state_info)
}
\author{
  john verzani
}
\seealso{
  \code{\link{gpanedgroup}} is a two-panel border layout
  with just an "east" or "south" satellite panel
  configured.
}

