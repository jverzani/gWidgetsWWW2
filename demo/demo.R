w <- gwindow("gWidgetsWWW2 examples")
sb <- gstatusbar("Powered by gWidgetsWWW2 and Rack", cont=w)
g <- gframe("Some basic examples for gWidgetsWWW2", horizontal=FALSE, cont=w)

glabel("This page shows the example files in the package's examples directory.", cont=g)

fs <- list.files(system.file("examples", package="gWidgetsWWW2"), full=TRUE)
basefs <- gsub("\\.R$", "", basename(fs))

sapply(seq_along(fs), function(i) {
  g1 <- ggroup(cont=g)

  gbutton("Source", cont=g1, action=i, handler=function(h,...) {
    w1 <- gwindow(sprintf("Source of %s", basefs[i]), width=450, height=400, parent=w)
    gw <- ggroup(cont=w1, spacing=0, horizontal=FALSE)
    lns <- readLines(fs[h$action])
    gcodemirror(paste(lns, collapse="\n"), expand=TRUE,  cont=gw)
    gseparator(cont=gw)
    bg <- ggroup(cont=gw)
    gbutton("dismiss", cont=bg, handler=function(h,...) dispose(w1))
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

