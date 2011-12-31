w <- gwindow("gedit")
g <- ggroup(cont=w, horizontal=FALSE)

## vanilla
e1 <- gedit("vanilla", cont=g)

## initial message
e2 <- gedit("", initial.msg="initial", cont=g)

## validation
e3 <- gedit("", validate.type="alpha", cont=g)

## validate regexp
e4 <- gedit("", validate.regexp=setNames("[0-9]+", "Only numbers"), cont=g)

## handler
e5 <- gedit("", initial.msg="type a value", cont=g, handler=function(h,...) {
  print("call handler")
})


## add a handler
e6 <- gedit("",cont=g)
addHandlerChanged(e6,  handler=function(h,...) {
  print("call handler")
})

## blur handler
e7 <- gedit("",cont=g)
addHandlerBlur(e7, handler=function(h,...) print("call handler"))

## "enter" handler
e8 <- gedit("", cont=g)
e8$add_handler_enter(function(...) print("call handler"))

## tests

## svalue
expect_equal(svalue(e1), "vanilla")

## svalue<-
svalue(e2) <- "added"
expect_equal(svalue(e2), "added")

## [<- typeahead. This is in combobox

## handler
expect_output(e6$invoke_change_handler(), "call handler")
