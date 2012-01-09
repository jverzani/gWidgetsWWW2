w <- gwindow("Google maps example")
sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
box <- ggroup(cont=w, horizontal=FALSE, use.scrollwindow=TRUE)
## Show map of central park
central_park <- c(40.782686, -73.965) ## central park

## make map
gm <- ggooglemaps(cont=box, center=central_park, zoom=12, height=300)

## Show how to click to add a polygon
g1 <- ggroup(cont=box)
glabel("Click some points, then click button to: ", cont=g1)
x <- numeric(0); y <- numeric(0)
b <- gbutton("add polygon", cont=g1, handler=function(h,...) {
  ggooglemaps_polygon(cbind(x, y), stroke=list(color="#FF00FF"), fill=list(color="#00FF00"), map=gm)
})

addHandlerClicked(gm, handler=function(h,...) {
  ggooglemaps_marker(position=c(h$lat, h$lng), title="", map=gm)
  x <<- c(x, h$lat); y <<- c(y, h$lng)
})


##How to add a marker and an icon along with a callback to open an info window
m <- ggooglemaps_marker(position=central_park, title="click me",
                        icon="http://google-maps-icons.googlecode.com/files/park-urban.png", map=gm)
addHandlerClicked(m, handler=function(h,...) {
  ggooglemaps_infowindow(h$obj, content = "Hello world", map=gm)
})


## Buttons to toggle traffic and bicycle overlays
g1 <- ggroup(cont=box)
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
