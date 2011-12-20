w <- gwindow("gcanvas example")
sb <- gstatusbar("Powered by gWidgetsWWW2 and Rook", cont=w)
addHandlerDestroy(w, handler=function(h,...) { dev.off() }) ## close canvas device
                  
g <- ggroup(cont=w, horizontal=FALSE)
ghtml(paste("The <code>gcanvas</code> constructor makes a widget to display graphics",
            "produced by the <code>canvas</code> device. This device writes JavaScript",
            "code to manipulate the web browser's canvas tag. To use <code>gcanvas</code>",
            "one simply passes the file of JavaScript commands to the object using <code>svalue</code>.",
            "The widget responds to mouse clicks and passes back the position of the click in ndc",
            "coordinates (in [0,1], with (0,0) being the lower left hand corner of the display) and pixel coordinates",
            "with (0,0) being the upper left. These are passed as x,y and X,Y components of the",
            "'h' list passed to the callback.",
            "In the example, a new point is added and the regression line redrawn.",
            sep=" "), cont=g)


x <- runif(3)
y <- runif(3)
width <- 400; height <- 300
f <- tempfile()
cnv <- gcanvas(f, width=width, height=height, cont=g)
canvas(f, width=width, height=height)                  
g1 <- ggroup(cont=g)
tpl <- "Plotting %s points"
lab <- glabel("", cont=g1)

gbutton("reset", cont=g, handler=function(h,...) {
  x <<- runif(3)
  y <<- runif(3)
  make_plot()
})

## script globals
setup_plot <- function() {
  xlim <- ylim <- c(0,1)
  plot.new()
  plot.window(xlim=xlim, ylim=ylim)
  axis(1); axis(2)
}

make_plot <- function() {
  canvas(f, width=width, height=height)
  setup_plot()
  points(x,y, pch=16, cex=2)
  abline(lm(y ~ x))
  dev.off()


  svalue(cnv) <- f
  svalue(lab) <- sprintf(tpl, length(x))
}

make_plot()

addHandlerClicked(cnv, handler=function(h,...) {
  ## redo plot to get grconvertXY to work...
  canvas(f, width=width, height=height)
  setup_plot()
  new.x <- grconvertX(h$x, from="ndc", to="user")
  new.y <- grconvertX(h$y, from="ndc", to="user")
  dev.off()
  
  if(min(new.x, new.y) < 0 || max(new.x, new.y) > 1)
    return()                            # too big for [0,1]
  x <<- c(x, new.x); y <<- c(y, new.y)
  make_plot()
})
               
