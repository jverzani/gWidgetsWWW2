## Test many different layouts

w <- gwindow("Test of different layouts")
gstatusbar("Powered by gWidgetsWWW2 and Rook", cont=w)

width <- 500
## ggroup
## ggroup horizontal works -- if -- we specify size for frames, ...
g <- gframe("Test of layouts: ggroup, gframe, gexpandgroup, gnotebook and glayout", cont=w, horizontal=FALSE)

fg <- gframe("horizontal ggroup", cont=g)
# horizontal group
f <- ggroup(cont=fg, horizontal = TRUE, width=width)
b <- gbutton("button1", cont=f)
addHandlerClicked(b, handler = function(h,...) gmessage("clicked",parent=b))
b1 <- gbutton("button 2", cont=f)


## frame
f1 <- gframe("gframe container with title",cont=g, horizontal=TRUE, width=width)
l <- gbutton("button 4", cont=f1)
font(l) <- c("font-weight"="bold")
b1 <- gbutton("button 4", cont=f1)

## gexpandgroup
f <- gexpandgroup("gexpandgroup: click bar to toggle visibility",cont=g, horizontal=TRUE, width=width)
b <- gbutton("button 5", cont=f)
b1 <- gbutton("button 6", cont=f)

## gnotebook
f <- gframe("gnotebook container", cont = g)
f <- gnotebook(cont=f,horizontal=TRUE)
size(f) <- c(width,400)
gbutton("button 7", cont = f, label = "tab 1")
gbutton("button 8", cont = f, label = "tab 2")
## tooltips when tabpos not on bottom (=1)
gbutton("button 9 (with tooltip)", cont=f, label="one",tooltip="hovering...")


## ## glayout
fg <- gframe("glayout example (restricted usage compared to other gWidgets)", cont = g)
f <- glayout(cont = fg)
f[1,1, anchor=c(1,0)] = "right aligned"
f[1,2] <- gbutton("button 10", cont=f)
f[2,1] <- "No empty spots allowed"
f[2,2] <- ''
f[3,1:2] <- "Use an empty label, as above"
visible(f) <- TRUE

visible(w) <- TRUE
