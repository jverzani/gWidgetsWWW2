w <- gwindow("Handler example")
sb <- gstatusbar("Powered by gWidgetsWWW2 and Rook", cont=w)
g <- gvbox(cont=w)
ghtml("This small example shows how one can add a handler, block its call, unblock the block, and remove the handler", cont=g)

g1 <- ggroup(cont=g)
b <- gbutton("click me for a message", cont=g1)

id <- addHandlerChanged(b, handler=function(h,...) {
  galert("I've been clicked...", parent=w)
})

b1 <- gbutton("block handler", cont=g1, handler=function(h,...) {
  blockHandler(b, id)
})
b1 <- gbutton("unblock handler", cont=g1, handler=function(h,...) {
  unblockHandler(b, id)
})
b1 <- gbutton("remove handler", cont=g1, handler=function(h,...) {
  removeHandler(b, id)
})
