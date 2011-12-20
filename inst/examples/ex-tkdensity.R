## tkdensity demo

w <- gwindow("tkdensity-type example")
gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)

g <- ggroup(cont=w, horizontal=FALSE)
ghtml("A web app similar to that of the tkdensity demo", cont=g)

pg <- gpanedgroup(cont=g,  height=450)

width <- height <- 400;
plotFile <- get_tempfile()

cnv <- gcanvas(plotFile, width=width, height=height, cont=pg)
fg <- ggroup(cont=pg, horizontal=FALSE)

## our filters
fs <- list()

f1 <- gframe("Distribution", cont=fg)
dens <- list("Normal"=rnorm, "Exponential"=rexp)
fs$distribution <- gradio(names(dens), cont=f1)

f1 <- gframe("Kernel", cont=fg)
kerns <- c("gaussian", "epanechnikov", "rectangular", "triangular", "cosine")
fs$kernel <- gtable(kerns, cont=f1)
svalue(fs$kernel, index=TRUE) <- 1      # isn't working? Sets value, not highlight

f1 <- gframe("Sample size", cont=fg)
fs$n <- gcombobox(c(50, 100, 200, 300), editable=TRUE, cont=f1)

f1 <- gframe("Bandwidth", cont=fg)
fs$bw <- gslider(from=5, to=200, by=5, tpl="{0}%", value=100, cont=f1)

gseparator(cont=fg)
gbutton("refresh", cont=fg, handler=function(h,...) {
  make_plot()
})


make_plot <- function(...) {
  canvas(plotFile, width=width, height=height)

  ## get values
  n <- as.numeric(svalue(fs$n))
  y <- dens[[svalue(fs$distribution)]](n)
  kern <- svalue(fs$kernel, index=FALSE)
  bw <- svalue(fs$bw)/100

  plot(density(y, bw=bw, kernel=kern))
  points(y,rep(0,length(y)))

  dev.off()
  svalue(cnv) <- plotFile
}

sapply(fs, addHandlerChanged, handler=make_plot)
