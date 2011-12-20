## Example where we use multiple gwindow's and the renderTo argument
## must be called with correct file
## one main window (no parent) per script

w <- gwindow("Sample app", renderTo="replaceme")
g <- ggroup(cont=w)
glabel("Enter some text", cont=g)
e <- gedit("", cont=g)

## second window. Needs parent argument

w1 <- gwindow(renderTo="replacemetoo", parent=w)
g <- ggroup(cont=w1)
b <- gbutton("Click to open subwindow", cont=g, handler=function(h,...) {

  ## no renderTo but has parent -- a subwindow
  w2 <- gwindow("A subwindow", parent=w)
  ghtml("A subwindow", cont=w2)
  
})
b1 <- gbutton("Click to view text", cont=g, handler=function(h,...) {
  gmessage(sprintf("you entered: %s", svalue(e)), parent=w)
})
             
