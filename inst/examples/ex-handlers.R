w <- gwindow("Handler example")
sb <- gstatusbar("Powered by gWidgetsWWW2 and Rook", cont=w)

ghtml("This small example shows how one can add a handler, blocks its call, unblock the block, and remove the handler", cont=w)

b <- gbutton("click me for a message", cont=w)

id <- addHandlerClicked(b, handler=function(h,...) {
  galert("I've been clicked...", parent=w)
})

b1 <- gbutton("block handler", cont=w, handler=function(h,...) {
  blockHandler(b1, id)
})
b1 <- gbutton("unblock handler", cont=w, handler=function(h,...) {
  unblockHandler(b1, id)
})
b1 <- gbutton("remove handler", cont=w, handler=function(h,...) {
  removeHandler(b1, id)
})
