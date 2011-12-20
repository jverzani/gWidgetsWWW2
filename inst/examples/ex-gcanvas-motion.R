w <- gwindow("gcanvas example")
sb <- gstatusbar("Powered by gWidgetsWWW2 and Rack", cont=w)
g <- ggroup(cont=w, horizontal=FALSE)
ghtml(paste("An example showing the canvas mouse move events when used with an R callback.",
             "They work -- but are not so responsive, as evidenced by the jerkiness below.",
             "Try clicking on a circle and dragging it around",
             sep=" "),
      cont=g)


width <- 500; height <- 450
cnv <- gcanvas(width=width, height=height, cont=g)

## a class to hold a circle
Circle <- setRefClass("Circle",
                      fields=list(
                        canvas = "ANY",
                        x="numeric",
                        y="numeric",
                        r="numeric",
                        col = "ANY",
                        styles="list"
                        ),
                      methods=list(
                        initialize=function(cnv, x, y, r, col=NULL, ...) {
                          canvas <<- cnv
                          x <<- x
                          y <<- y
                          r <<- r
                          col <<- col
                          styles <<- list(...)
                          .self
                        },
                     
                        dist = function(a, b=NULL) {
                          "Euclidean distance between a,b and x,y"
                          if(!is.null(b))
                            a <- c(a,b)
                          d2 <- sum( (a[1] - x)^2 + (a[2] - y)^2)
                          sqrt(d2)
                        },
                        contains = function(a, b=NULL) {
                          if(!is.null(b))
                            a <- c(a,b)
                          dist(a) < r
                        },
                        draw = function() {
                          l <- list(x=x, y=y, r=r, col=col)
                          l <- gWidgetsWWW2:::merge.list(l, styles)
                          do.call(canvas$circle, l)
                        }
                        )
                      )
          
circleList <- gWidgetsWWW2:::Array$new() ## helper class
x <- seq(from=50, to=350, by=50)
y <- x
r <- 25

sapply(seq_along(x), function(i) {
  circleList$push(Circle$new(cnv, x[i], y[i], r))
})

## draw the circles
circleList$each(function(i, key, value) value$draw())

## Add in motion
closest <- NULL
cnv$add_handler_mouse_down(handler=function(h,...) {
  i <- which.min(circleList$each(function(i, key, value) value$dist(h$X, h$Y)))
  tmp <- circleList$get_item(i)
  if(tmp$contains(h$X, h$Y)) {
    closest <<- tmp
    closest$col <- "#FF00FF"
    ## draw
    cnv$call_method("clearRect", 0,0, width, height)
    circleList$each(function(i, key, value) value$draw())
  }
})


cnv$add_handler_mouse_move(handler=function(h,...) {
  if(!is.null(closest)) {
    ## update position
    closest$x <- h$X
    closest$y <- h$Y
    ## draw
    cnv$call_method("clearRect", 0,0, width, height)
    circleList$each(function(i, key, value) value$draw())
  }
})

cnv$add_handler_mouse_up(handler=function(h,...) {
  ## clean up
  closest$col <- NULL
  closest <<- NULL
  ## redraw with updated colors
  cnv$call_method("clearRect", 0,0,width, height)
  circleList$each(function(i, key, value) value$draw())

})


