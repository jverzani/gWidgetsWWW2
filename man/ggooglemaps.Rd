\name{ggooglemaps}
\alias{ggooglemaps}
\title{Google maps widget}
\usage{
  ggooglemaps(center = c(45, 45), zoom = 13, maptype =
  "roadmap", container, ..., width = NULL, height = 400,
  ext.args = NULL)
}
\arguments{
  \item{center}{lat/lng pair where map should be centered}

  \item{zoom}{zoom level for initial map}

  \item{maptype}{Type of map}

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

  \item{width}{width in pixels of component. Sizing in
  \pkg{gWidgetsWWW2} is sometimes necessary as the
  arguments \code{expand} and \code{fill} are not well
  implemented.}

  \item{height}{height in pixels of the component.}

  \item{ext.args}{The gWidgets' constructors ultimately
  call an Ext constructor. The options passed to the Ext
  constructor may be added to or overridden by use of this
  argument. Values are passed in a named list components
  and conversions to JavaScript objects}
}
\value{
  an GWidget object
}
\description{
  Google maps widget
}
\details{
  Widget to display a google map and expose some of the
  google maps API through R methods
}
\examples{
\dontrun{
w <- gwindow("hello", renderTo="replaceme")
sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)

## Show map of central park
central_park <- c(40.782686, -74.01) ## central park

## make map
gm <- ggooglemaps(cont=w, center=central_park, zoom=12, height=400)

## Show how to click to add a polygon
g1 <- ggroup(cont=w)
glabel("Click some points, then click button to: ", cont=g1)
x <- numeric(0); y <- numeric(0)
b <- gbutton("add polygon", cont=g1, handler=function(h,...) {
ggooglemaps_polygon(cbind(x, y), stroke=list(color="#FF00FF"), fill=list(color="#00FF00"), map=gm)
})
addHandlerClicked(gm, handler=function(h,...) {
ggooglemaps_marker(position=c(h$lat, h$lng), title="", map=gm)
x <<- c(x, h$lat); y <<- c(y, h$lng)
})
## How to add a marker with a callback to open an info window
m <- ggooglemaps_marker(position=central_park, title="click me", map=gm)
addHandlerClicked(m, handler=function(h,...) {
ggooglemaps_infowindow(h$obj, content = "Hello world", map=gm)
})


## Buttons to toggle traffic and bicycle overlays
g1 <- ggroup(cont=w)
glabel("Change map type", cont=g1)
gcombobox(c("roadmap", "satellite", "hybrid", "terrain"), cont=g1, handler=function(h,...) {
gm$set_maptype(svalue(h$obj, index=FALSE))
})
gcheckbox("Traffic overlay", checked=FALSE, width=150, cont=g1, handler=function(h,...) {
gm$add_trafficlayer(svalue(h$obj))
})
gcheckbox("Bicycle overlay", checked=FALSE, width=150, cont=g1, handler=function(h,...) {
gm$add_bikelayer(svalue(h$obj))
})

}
}

