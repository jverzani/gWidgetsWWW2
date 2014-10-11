w <- gwindow("gWidgetsWWW2 examples")
sb <- gstatusbar("Powered by Rook and gWidgetsWWW2", cont=w)
g <- gframe("Some basic examples of gWidgetsWWW2", use.scrollwindow=TRUE, horizontal=FALSE, cont=w)

glabel("This page shows the example files in the package's examples directory.", cont=g)

fs <- list.files(system.file("examples", package="gWidgetsWWW2"), full=TRUE)
basefs <- gsub("\\.R$", "", basename(fs))

sapply(seq_along(fs), function(i) {
    ## work around stretching
  g1 <- ggroup(cont=g, ext.args=list(layout=list(type="hbox", align="top")))

  gbutton("Source", cont=g1, action=i, handler=function(h,...) {
    w1 <- gwindow(sprintf("Source of %s", basefs[i]), width=650, height=400, parent=w)
    gw <- ggroup(cont=w1, spacing=0, horizontal=FALSE)
    lns <- readLines(fs[h$action])
    cm <- gcodemirror(paste(lns, collapse="\n"), expand=TRUE,  cont=gw)
    cm$set_editable(FALSE)
    gseparator(cont=gw)
    bg <- ggroup(cont=gw)
    gbutton("dismiss", cont=bg, handler=function(h,...) dispose(w1))
  })

  gbutton("View example", cont=g1, action=i, handler=function(h,...) {
    message("load_app in demo")
    load_app(fs[h$action], gsub("-", "_", basefs[h$action]))
  })

  glabel(basefs[i], cont=g1)
})


## If you see this, you should try demo("gWidgetsWWW2") instead.  

