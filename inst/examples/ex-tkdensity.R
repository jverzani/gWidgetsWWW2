## tkdensity demo
require(RSVGTipsDevice)

w <- gwindow("tkdensity-type example")
gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)

bl <- gborderlayout(cont=w)
ghtml("A web app similar to that of the tkdensity demo", cont=bl, where="north")

width <- 500; height <- 400;
plotFile <- get_tempfile(ext=".svg")

cnv <- gsvg(plotFile, width=width, height=height, cont=bl, where="center")

fg <- ggroup(cont=bl, horizontal=FALSE, where="west", use.scrollwindow=TRUE)
bl$set_panel_size("west", 200)

## our filters
fs <- list()

gbutton("refresh", cont=fg, handler=function(h,...) {
  make_plot()
})

GLABEL <- function(txt, ...) ghtml(sprintf("<b>%s</b>",txt), ...)

GLABEL("Distribution", cont=fg)
dens <- list("Normal"=rnorm, "Exponential"=rexp)
fs$distribution <- gradio(names(dens), cont=fg)

GLABEL("Sample size", cont=fg)
fs$n <- gcombobox(c(50, 100, 200, 300), selected=1, editable=TRUE, cont=fg,
                  coerce.with=as.numeric)

GLABEL("Bandwidth", cont=fg)
fs$bw <- gslider(from=5, to=200, by=5, tpl="{0}%", value=100, cont=fg)



GLABEL("Kernel", cont=fg)
kerns <- data.frame(kernels=c("gaussian", "epanechnikov", "rectangular", "triangular", "cosine"), stringsAsFactors=FALSE)
fs$kernel <- gtable(kerns, cont=fg, height=150, expand=FALSE)
svalue(fs$kernel, index=TRUE) <- 1      # isn't working? Sets value, not highlight

## size them
sapply(fs, function(i) size(i) <- list(width=200))



make_plot <- function(...) {
  svg(plotFile, width=width, height=height)

  ## get values
  n <- svalue(fs$n)
  y <- dens[[svalue(fs$distribution)]](n)
  kern <- svalue(fs$kernel, index=FALSE)
  bw <- svalue(fs$bw)/100

  plot(density(y))#, bw=bw, kernel=kern))
  points(y,rep(0,length(y)))

  dev.off()
  svalue(cnv) <- plotFile
}


sapply(fs, addHandlerChanged, handler=make_plot)
