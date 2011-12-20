## exampe of graph, data viewer, filter
require(MASS)
d <- Cars93[c(1,2, 5, 8, 19, 21, 25, 10)]
d.shown <- d

height <- 500

w <- gwindow("Filter, graph, table")
gstatusbar("Powered by gWidgetsWWW2 and Rook", cont=w)

nb <- gnotebook(cont=w, height=height)
ghtml(paste("An example of three widgets: a data table (<code>gtable</code>)",
            "a graphic (<code>gcanvas</code>) and a filter view.",
            sep="<br /> "),

      cont=nb,
      label="About")

pg <- gpanedgroup(horizontal=TRUE, cont=nb, height=height,label="Example")
pgl <- gpanedgroup(horizontal=FALSE, cont=pg)
fg <- ggroup(horizontal=FALSE, cont=pg)
svalue(nb) <- 2

## set up canvas
f <- tempfile()
cwidth <- 450; cheight <- 300 # canvas dimensions
cnv <- gcanvas(f, width=cwidth, height=cheight, cont=pgl)

## table widget
tbl <- gtable(d, multiple=TRUE, cont=pgl)

## Set up some filters

f1 <- gframe("Length <=", cont=fg, horizontal=FALSE)
len <- gslider(from=min(d$Length), to=max(d$Length), by=1,
               value=max(d$Length), cont=f1)


f2 <- gframe("Weight <=", cont=fg, horizontal=FALSE)
wt <- gslider(from=min(d$Weight), to=max(d$Weight), by=1,
               value=max(d$Weight), cont=f2)

f3 <- gframe("Drive Train", cont=fg, horizontal=FALSE)
dt <- gcheckboxgroup(unique(levels(d$DriveTrain)), checked=TRUE,
                     horizontal=FALSE,
                     cont=f3)

visible(w) <- TRUE

## Actions.
## No MVC framework setup, so we do this by hand

make_plot <- function(...) {
  ## get values
  theLenth <- svalue(len)
  theWeight <- svalue(wt)
  theDriveTrain <- svalue(dt)

  ind <- with(d,Length <= svalue(len) &
                Weight <= svalue(wt) &
                DriveTrain %in% svalue(dt)
                )
  d.cur <<- d[ind,]
  
  ## update table
  svalue(tbl, index=TRUE) <- which(ind)

  ## update plot
  canvas(f, width=cwidth, height=cheight)
  if(any(ind)) {
    plot(MPG.highway ~ Price, data=d[ind,])
  } else {
    plot(MPG.highway ~ Price, data=d, type="n")
  }
  dev.off()
  svalue(cnv) <- f
}


sapply(list(len, wt, dt), addHandlerChanged, handler=make_plot)

## canvas click
locate_point <- function(x, y, pt) {
  ## Find point in d from ndc coordinates
  m <- rbind(x,y)
  ds <- sapply(1:ncol(m),
              function(i,z) sum((m[,i]-z)^2),
              z=pt)
  ind <- min(which.min(ds))
  return(list(index=ind, distance=ds[ind], x=x[ind], y=y[ind]))
}

addHandlerClicked(cnv, handler=function(h,...) {
  ## need to set up device for grconvertXY to work.
  canvas(tempfile(), width=cwidth, height=cheight)
  plot(MPG.highway ~ Price, data=d, type="n")
  pt <- c(grconvertX(h$x, from="ndc", to="user"), grconvertY(h$y, from="ndc", to="user"))
  dev.off()
  
  l <- locate_point(d.cur$Price, d.cur$MPG.highway, pt)
  tpl <- sprintf("You clicked nearest the %s %s", d.cur$Manufacturer[l$ind], d.cur$Model[l$ind])
  galert(tpl, parent=w)
})
## initial plot
make_plot()
