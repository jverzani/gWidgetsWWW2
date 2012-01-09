w <- gwindow("Hello world example")
sb <- gstatusbar("Powered by Rook and gWidgetsWWW2", cont=w)

g <- ggroup(cont=w, horizontal = FALSE, use.scrollwindow=TRUE)

width <- 500


f <- gframe("gbutton", expand=TRUE, cont=g)
gbutton("Click me", cont=f, handler = function(h,...) {
  gmessage("hello world", parent = w)
})


f <- gframe("gedit", cont=g)
glabel("Type in box, then leave it: ", cont=f)
gedit("", cont=f, handler = function(h,...) {
  gmessage(sprintf("Hello world, you typed: %s", svalue(h$obj)), parent=w)
})


f <- gframe("gcombobox", cont=g)
gcombobox(c("world", "country","planet"), selected=1, cont=f, handler = function(h,...) {
  gmessage(sprintf("Hello world, you selected: %s", svalue(h$obj)), parent=w)
})




f <- gframe("gcheckbox", cont=g)
## expand to fill the horizontal space, otherwise size of checkbox is not large
gcheckbox("hello", checked=FALSE, cont=f, expand=TRUE, handler=function(h,...) {
  if(svalue(h$obj))
    gmessage("Hello world", parent=w)
})

f <- gframe("gradio", cont=g)
rb <- gradio(c("world", "country","planet"), cont=f, expand=TRUE)
## change handler called on load -- irksome so we use galert
addHandlerChanged(rb, handler=function(h, ...) {
  galert(sprintf("Hello %s", svalue(h$obj)), parent=w)
})


f <- gframe("gcheckboxgroup", cont=g)
gcheckboxgroup(c("world", "country","planet"), cont=f, expand=TRUE, handler=function(h, ...) {
  gmessage(sprintf("Hello %s", paste(svalue(h$obj), collapse=", ")), parent=w)
})

f <- gframe("gslider", cont=g)
## need a width
gslider(value=50, cont=f, width=200, handler=function(h,...) {
  gmessage(sprintf("Hello %s percent of world", svalue(h$obj)), parent=w)
})

f <- gframe("gspinbox", cont=g)
gspinbutton(value=50, cont=f, handler=function(h,...) {
  gmessage(sprintf("Hello %s percent of world", svalue(h$obj)), parent=w)
})


f <- gframe("gtable", horizontal=FALSE, cont=g)
m <- data.frame(greeting=rep("hello", 3), who=c("world","planet","country"), stringsAsFactors=FALSE)
gtable(m, cont=f, multiple=FALSE, height=100, handler=function(h,...) {
  ind <- svalue(h$obj, index=TRUE)
  gmessage(sprintf("Hello %s", h$obj[ind,2]), parent=w)
})


f <- gframe("gdf, just for show, no handler", cont=g)
m <- data.frame(greeting=rep("hello", 3), who=c("world","planet","country"), stringsAsFactors=FALSE)
gdf(m, cont=f)

