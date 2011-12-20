## Example of how widgets can be added dynamically, that is from
## within a handler

w <- gwindow("Add widgets example")
g <- ggroup(cont = w, horizontal=FALSE)
ghtml(paste("A simple example of adding a widget dynamically to a GUI.",
            "When a non-blank value for the combobox is set, a new combobox is added.",
            sep=" "),
      cont=g)

## simple way to see that things have changed
gbutton("click to see stored value(s)", cont = g, handler = function(h,...) {
  out <- sapply(l, svalue)
  galert(paste(out, collapse="  "), parent=w)
})

## we keep this as a global and use <<- within the handler
l <- list()
addCombobox <- function(g) {
  g1 <- ggroup(cont = g)
  glabel("label", cont = g1)
  l[[length(l) + 1]] <<- gcombobox(c("",letters), cont = g1, handler = function(h,...) {
    out <- sapply(l, svalue)
    if(!any(sapply(out, function(i) i == "")))
      addCombobox(g)
  })
}

addCombobox(g)

## show off
gstatusbar("Powered by RApache and gWidgetsWWW", cont = w)
visible(w) <- TRUE
