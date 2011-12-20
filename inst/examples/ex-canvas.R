w <- gwindow("Example of canvas device within gWidgetsWWW")
g <- ggroup(cont = w, horizontal=FALSE)

width <- height <- 400

ghtml(paste("Example of using the canvas device with gWidgetsWWW. Requires HTML5-compliant browser to work.",
            "<br>",
            "The canvas device uses javascript to write the graphic, not an image.",
            "It is an alternative to the gsvg device, which requires its image to be loaded from a file.",
            collapse="\n"), cont=g)

##' make a plot. This would, of course, be modified
makePlot <- function() {
  ## load packaage quietly
  require(canvas, quietly=TRUE, warn=FALSE)
  f <- get_tempfile(ext=".canvas")
  canvas(f, width=width, height=height, bg="#ffffff") # transparent will allow overplot
  hist(rnorm(100))
  dev.off()
  f
}

if(!require(canvas)) {
  glabel("This needs the canvas package to be installed", cont=g)
} else {
  ## Use a canvas widget
  canvas <- gcanvas(cont = g, width=width, height=height)
  svalue(canvas) <- makePlot()
  b <- gbutton("click me for another", cont = g, handler = function(h,...) {
    svalue(canvas) <- makePlot()
  })
  
}

## add status bar and show off
gstatusbar("Powered by RApache and gWidgetsWWW", cont = w)
visible(w) <- TRUE
