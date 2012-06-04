## example of gtable widget THe gtable widget is *picky* about sizing,
## including the parent container it is packed into.



w <- gwindow("Examples of gtable widget", visible=FALSE)
gstatusbar("Powered by gWidgetsWWW2 and Rook", cont=w)


d <- mtcars[1:3, 1:6]

g <- ggroup(cont=w, horizontal = FALSE, use.scrollwindow=TRUE)

## single vector
f <- gframe("Works with single column, but can feed data frame too. (Default size)",
            cont=g, expand=TRUE)
tbl <- gtable(state.name[1:3], cont=f)

## ## stock icons
m <- data.frame(icon=asIcon(c("ok","ok","dismiss")), state=state.name[1:3], stringsAsFactors=FALSE)
f <- gframe("icons added with asIcon column", cont=g, expand=TRUE)
tbl <- gtable(m, cont=f)
size(tbl) <- c(500, 100)



## [<- method
f <- gframe("can reset values to choose from", horizontal=FALSE,cont=g, expand=TRUE)
b <- gbutton("Click me to shorten to two rows", cont=ggroup(cont=f))
tbl <- gtable(d, cont=f);
size(tbl) <- c(500,200)
addHandlerClicked(b, handler=function(h,...) {
  h$action[] <- d[1:2,]
}, action=tbl)

## ## click handler
f <- gframe("addHandlerClick for selection click, also add handler double click", cont=g, expand=TRUE)
tbl <- gtable(d, cont=f)
size(tbl) <- c(500,100)
addHandlerClicked(tbl, handler=function(h,...) {
  gmessage(sprintf("Value: %s, index: %s", svalue(h$obj, index=FALSE), svalue(h$obj, index=TRUE)), parent=h$obj)
})

## ## ## sorting
## ## XXX Sorting seems broken as of 4.1.0-beta
## ## f <- gframe("Can sort by clicking on headers. -- Try it", cont=g)
## ## tbl <- gtable(d, cont=f)
## ## size(tbl) <- c(500,100)
## ## addHandlerClicked(tbl, handler=function(h,...) {
## ##   gmessage(sprintf("Value: %s, index: %s", svalue(h$obj, index=FALSE), svalue(h$obj, index=TRUE)), parent=h$obj)
## ## })

## Filteriing
f <- gframe("Filtering is done through the visible assignment method.", horizontal=FALSE, cont=g, expand=TRUE)
f1 <- ggroup(cont=f)
glabel("No Cyls:", cont=f1)
cb <- gcombobox(c("<All>", sort( unique(mtcars$cyl))), cont=f1)
tbl <- gtable(mtcars[, 1:5], cont=f)
size(tbl) <- c(500,300)
addHandlerChanged(cb, handler=function(h,...) {
  val <- svalue(h$obj)
  cyls <- h$action[,'cyl']
  if(val == "<All>")
    h$action$set_visible(rep(TRUE, length(cyls)))
  else
    h$action$set_visible(cyls %in% val)
}, action=tbl)

## ## Paging -- XXX
## f <- gframe("Example of paging for handling large data sets", expand=TRUE,  cont=g)
## library(MASS)
## tbl <- gtable(Aids2, paging=TRUE, cont=f)
