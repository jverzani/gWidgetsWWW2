w <- gwindow("hello")
sb <- gstatusbar("Powered by gWidgetsWWW and Rack", cont=w)
b <- gbutton("poke", cont=w, handler=function(h,...) {

})
b <- gbutton("peek", cont=w, handler=function(h,...) {
})

require(canvas)
width <- height <- 400
f <- tempfile()
canvas(f, width=width, height=height)
plot(1:5, 5:1, pch=16, cex=2)
dev.off()

g <- ggroup(cont=w)                     # horizontal
glabel("shift to right", cont=g)
canv <- gcanvas(f, width=width, height=height, cont=g)

addHandlerClicked(canv, handler=function(h,...) {
  gmessage(paste(h$x, h$X, h$y, h$Y, sep="--"), parent=w)
})


## tbl <- glayout(cont=w, spacing=2)
## tbl[1,1, anchor=c(1,0)] = "fred"
## tbl[1,2, anchor=c(-1,0)] = gedit("", cont=tbl)
## tbl[2,1, anchor=c(1,0)] = "barnety"
## tbl[2,2, anchor=c(-1,0)] = gedit("", cont=tbl)
## visible(tbl) <- TRUE



## source("~/pmg/gw2/gprocessingjs.R")

## f <- gframe("Basic example showing histogram", height=400, horizontal=FALSE, container = g)
## p <- gprocessingjs(width=500, height = 400, container  = f)
## Hist <- function(p,x, breaks = "Sturges", col = "goldenrod") {
##   out <- hist(x, breaks = breaks, plot=FALSE)
##   p$plot.new()
## #  p$plot.window(xlim = range(out$breaks), ylim = c(0, max(out$counts)))
## #  p$axis(1)
## #  p$title(main = deparse(substitute(x)))
## #  nb <- length(out$breaks)
## #  p$rect(out$breaks[1:(nb-1)], rep(0, length=nb-1), out$breaks[2:nb],
## #    out$counts, col)
## }
## Hist(p, faithful$eruptions)

##h <- ghtml("http://www.yahoo.com", cont=g)

## g <- gexpandgroup("fred", cont=w, horizontal=FALSE) ##ggroup(cont=w, horizontal=TRUE)

## stub <- function(h,...) {galert("hi", parent=w)}
## l <- list(File=list(a1=gaction("fred", handler=stub, parent=w),
##             a2=gaction("fred", handler=stub, parent=w),
##             b =gseparator(),
##             a3=gaction("fred", handler=stub, parent=w)),
##           Edit = list(a=gaction("fred", handler=stub, parent=w))
##           )
## a <- gaction("fred", handler=function(h,...) {svalue(l) <- "new text"}, parent=w)
## gmenu(l, cont=w)

## b <- gbutton(action=a, container=g)

## l <- glabel("My label", container=g)

##  for(i in 1:3)
##    b <- gbutton("click me", cont=g)
## g1 <- ggroup(cont=g, horizontal=FALSE)
## for(i in 1:3)
##   b1 <- gbutton("click me", cont=g1)

## b <- gbutton("Click for message", tooltip="be careful",
##              cont=g, handler=function(h,...) {
## ##   h$toplevel$js_queue_push("alert('hiya')")
##    svalue(b1) <- svalue(e)
## })

## e = gedit("asdf", cont=g)
## #b$add_js_callback("mouseover", sprintf("function() {%s.setText('changed')}", b$get_id()))

## w1 <- gwindow("hello", renderTo="replacemetoo", parent=w)
## b <- gbutton("another button", cont=w1, handler=function(h,...) {
##   svalue(b1) <- "wilma"
##  })

## b <- gbutton("click me", cont=g, handler=function(h,...) {
##   ##svalue(cb, index=TRUE) <- 1:2
##   svalue(cb) <- !svalue(cb)
## })

## m <- state.name[1:3]
## cb <- gcheckboxgroup(m[1], cont=g, handler=function(h,...) {
##   svalue(b) <- paste(svalue(cb), collapse="--")

##   })

## cb1 <- gcheckbox("fred", cont=g, width=100, handler=function(h,...) {
##   if(svalue(h$obj)) {
##     svalue(b) <- "checked"
##   } else {
##     svalue(b) <- "not checked"
##   }
## })


### How to create a new widget
## a <- ExtWidget$new(w$toplevel)
## a$constructor <- "Ext.ux.Canvas"
## a$add_args(list(width=400, height=400))
## a$write_constructor()
## g$add(a)


## ## Canvas example
## cnv <- gcanvas(cont=g, width=250, height=250)
## X <- sample(1:150, 10)
## Y <- sample(1:250, 10)
## f <- tempfile()
## require(canvas)
## canvas(f, width=250, height=250)
## plot(X,Y, pch=15)
## dev.off()
## svalue(cnv) <- f

## addHandlerClicked(cnv, function(h,...) {
##   ## h$x and h$y pass back mouse position -- although unreliably
##   ## should be in [0,width], [0,height] with (0,0) top left corner
##   x <- h$x; y <- h$y 
##   svalue(b) <- h$y
## })

#cnv$add_R_callback("click", callback=function(h,...) {
#  svalue(b) <= "clicked"
#})

## ## rb <- gradioa(m, cont=g, handler=function(h,...) {
## ##   val <- svalue(h$obj, index=FALSE)
## ##   h$obj$toplevel$js_queue_push(sprintf("alert('%s')", val))

## ##   })


## ## Comobobox
## ## ctr <- 9
## ## cb = gcombobox(m, cont=g, editable=TRUE)
## ## b1 <- gbutton("click", cont=g, handler=function(h,...) {
## ## #  svalue(cb, index=FALSE) <- "Arizona"
## ##   cb$set_items(state.name[1:ctr])
## ##   ctr <<- ctr - 1
## ## })
## visible(w) <- TRUE
