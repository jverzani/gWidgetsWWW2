## exampe of graph, data viewer, filter
require(MASS)
require(RSVGTipsDevice)

d <- Cars93[c(1,2, 5, 8, 19, 21, 25, 10)]
d.shown <- d

## dimensions
height <- 500
cwidth <- 450; cheight <- 400 # canvas dimensions


w <- gwindow("Filter, graph, table")
gstatusbar("Powered by Rook and gWidgetsWWW2", cont=w)

## an example of three widgets in a border layout
## had issue nesting borderlayout within a notebook

bl <- gborderlayout(cont=w)


## set up canvas
f <- get_tempfile(ext=".svg")
svg(f)
hist(rnorm(100))
dev.off()
cnv <- gsvg(f, width=cwidth, height=cheight, cont=bl, where="center")

## table widget
tbl <- gtable(d, multiple=TRUE, expand=TRUE,  cont=bl, where="east")

## Set up some filters
fg <- gframe("Filter by:", horizontal=FALSE, cont=bl, where="west")



filter_widgets <- list()
f1 <- gframe("Length <=", cont=fg, horizontal=FALSE)
filter_widgets$len <- gslider(from=min(d$Length), to=max(d$Length), by=1,
               value=max(d$Length), cont=f1)


f2 <- gframe("Weight <=", cont=fg, horizontal=FALSE)
filter_widgets$wt <- gslider(from=min(d$Weight), to=max(d$Weight), by=1,
               value=max(d$Weight), cont=f2)

f3 <- gframe("Drive Train",cont=fg, horizontal=FALSE)
filter_widgets$dt <- gcheckboxgroup(unique(levels(d$DriveTrain)), checked=TRUE,
                     horizontal=FALSE,
                     cont=f3)

## adjust sizes
sapply(c("east", "west"), bl$set_panel_size, 200)
sapply(filter_widgets, function(i) size(i) <-  list(width=200))

## Actions.

make_plot <- function(...) {
  ## get values
  l <- setNames(lapply(filter_widgets, svalue), c("theLength", "theWeight", "theDriveTrain"))
  
  ind <- with(d,Length <= l$theLength &
              Weight <= l$theWeight &
              DriveTrain %in% l$theDriveTrain
              )
  d.cur <<- d[ind,]
  
  ## update table
   svalue(tbl, index=TRUE) <- which(ind)

   ## update plot
   svg(f, width=cwidth, height=cheight)
  if(any(ind)) {
    plot(MPG.highway ~ Price, data=d[ind,])
  } else {
    plot(MPG.highway ~ Price, data=d, type="n")
  }
  dev.off()
  svalue(cnv) <- f
}


sapply(filter_widgets, addHandlerChanged, handler=make_plot)

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
