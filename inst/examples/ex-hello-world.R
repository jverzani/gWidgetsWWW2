w <- gwindow("Hello world example")
sb <- gstatusbar("Powered by gWidgetsWWW2 and rapache", cont=w)

g <- ggroup(cont=w, horizontal = FALSE)

f <- gframe("gbutton", cont=g)
gbutton("Click me", cont=f, handler = function(h,...) {
  galert("hello world", parent = w)
})


f <- gframe("gedit", cont=g)
glabel("Type in box, then leave it: ", cont=f)
gedit("", cont=f, handler = function(h,...) {
  gmessage(sprintf("Hello world, you typed: %s", svalue(h$obj)), parent=w)
})


f <- gframe("gcombobox", cont=g)
gcombobox(c("world", "country","planet"),, cont=f, handler = function(h,...) {
  gmessage(sprintf("Hello world, you selected: %s", svalue(h$obj)), parent=w)
})




f <- gframe("gcheckbox", cont=g)
gcheckbox("hello", checked=FALSE, cont=f, handler=function(h,...) {
  if(svalue(h$obj))
    gmessage("Hello world", parent=w)
})
f <- gframe("gradio", cont=g)
gradio(c("world", "country","planet"), cont=f, handler=function(h, ...) {
  gmessage(sprintf("Hello %s", svalue(h$obj)), parent=w)
})


f <- gframe("gcheckboxgroup", cont=g)
gcheckboxgroup(c("world", "country","planet"), cont=f, handler=function(h, ...) {
  gmessage(sprintf("Hello %s", paste(svalue(h$obj), collapse=", ")), parent=w)
})

f <- gframe("gslider", cont=g)
gslider(cont=f, handler=function(h,...) {
  gmessage(sprintf("Hello %s percent of world", svalue(h$obj)), parent=w)
})

f <- gframe("gspinbox -- not right!", cont=g)
gspinbutton(cont=f, handler=function(h,...) {
  gmessage(sprintf("Hello %s percent of world", svalue(h$obj)), parent=w)
})

f <- gframe("gtable", cont=g)
m <- data.frame(greeting=rep("hello", 3), who=c("world","planet","country"), stringsAsFactors=FALSE)
gtable(m, cont=f, multiple=FALSE, handler=function(h,...) {
  ind <- svalue(h$obj, index=TRUE)
  gmessage(sprintf("Hello %s", h$obj[ind,2]), parent=w)
})

f <- gframe("gdf, just for show", cont=g)
m <- data.frame(greeting=rep("hello", 3), who=c("world","planet","country"), stringsAsFactors=FALSE)
gdf(m, cont=f)






