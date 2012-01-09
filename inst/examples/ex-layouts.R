## Test many different layouts

w <- gwindow("Test of different layouts")
gstatusbar("Powered by gWidgetsWWW2 and Rook", cont=w)

## ggroup
## ggroup horizontal works -- if -- we specify size for frames, ...
g <- gframe("Test of layouts: ggroup, gframe, gexpandgroup, gnotebook and glayout", cont=w, use.scrollwindow=TRUE, horizontal=FALSE)

fg <- gframe("horizontal ggroup", horizontal=FALSE, expand=TRUE,  cont=g)
# horizontal group
bg <- ggroup(cont=fg, horizontal = TRUE)
b <- gbutton("button1", cont=bg)
addHandlerClicked(b, handler = function(h,...) gmessage("clicked",parent=b))
b1 <- gbutton("button 2", cont=bg)


## frame
f1 <- gframe("gframe container with title",cont=g, horizontal=FALSE, expand=TRUE)
bg <- ggroup(cont=f1)
l <- gbutton("button 4", cont=bg)
font(l) <- c("font-weight"="bold")
b1 <- gbutton("button 4", cont=bg)

## gexpandgroup
f <- gexpandgroup("gexpandgroup: click bar to toggle visibility",cont=g, horizontal=FALSE, expand=TRUE)
bg <- ggroup(cont=f)
b <- gbutton("button 5", cont=bg)
b1 <- gbutton("button 6", cont=bg)

## gnotebook
f <- gframe("gnotebook container", cont = g, horizontal=FALSE,height=100)
nb <- gnotebook(cont=f,horizontal=TRUE)
gbutton("button 7", cont = nb, label = "tab 1")
gbutton("button 8", cont = nb, label = "tab 2")
## tooltips when tabpos not on bottom (=1)
gbutton("button 9 (with tab tooltip)", cont=nb, label="one",tooltip="hovering...")


## ## glayout
fg <- gframe("glayout example (restricted usage compared to other gWidgets)", cont = g)
f <- glayout(cont = fg)
f[1,1, anchor=c(1,0)] = "right aligned"
f[1,2] <- gbutton("button 10", cont=f)
f[2,2] <- 'no 2,1 item'
f[3,1:2] <- "span 2 entries because this is long"
visible(f) <- TRUE

