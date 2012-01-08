w <- gwindow("gWidgetsWWW2 examples")
sb <- gstatusbar("Powered by gWidgetsWWW2 and Rack", cont=w)
g <- gframe("Some basic examples for gWidgetsWWW2", horizontal=FALSE, cont=w)

glabel("This page shows the example files in the package's examples directory.", cont=g)

fs <- list.files(system.file("examples", package="gWidgetsWWW2"), full=TRUE)
basefs <- gsub("\\.R$", "", basename(fs))

## Can't use sapply here if we return gWidgetsWWW2 objects, as the call to simplify2array
## tries to call the length.ExtWidget method
lapply(seq_along(fs), function(i) {
  g1 <- ggroup(cont=g)

  gbutton("Source", cont=g1, action=i, handler=function(h,...) {
    lns <- readLines(fs[h$action])
    w1 <- gwindow(sprintf("Source of %s", basefs[i]), width=450, height=400, parent=w)
    gw <- ggroup(cont=w1, horizontal=FALSE)
    ghtml(sprintf("<pre>%s</pre>", paste(lns, collapse="\n")), height=375, cont=gw)
    gbutton("dismiss", cont=gw, handler=function(h,...) dispose(w1))
  })

  gbutton("View example", cont=g1, action=i, handler=function(h,...) {
    if(grepl("multiple", basefs[h$action])) {
      brew_template <- system.file("framework/brew/custom.rhtml", package="gWidgetsWWW2")
      load_app( fs[h$action], gsub("-", "_", basefs[h$action]),  brew_template)
    } else {
      load_app(fs[h$action], gsub("-", "_", basefs[h$action]),  use.googlemap=TRUE)
    }
  })

  glabel(basefs[i], cont=g1)
})


## If you see this, you should try demo("gWidgetsWWW2") instead.  

