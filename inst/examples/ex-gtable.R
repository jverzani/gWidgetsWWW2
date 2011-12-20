## example of gtable widget



w <- gwindow("Examples of gtable widget", visible=FALSE)
gstatusbar("Powered by gWidgetsWWW2 and Rook", cont=w)


d <- mtcars[1:3, 1:6]


g <- ggroup(cont=w, horizontal=FALSE)
glabel(svalue(w), cont=g)

## single vector
f <- gframe("Works with single column, but can feed data frame too. (Default size)", cont=g)
tbl <- gtable(state.name[1:3], cont=f)

## ## stock icons
m <- data.frame(icon=asIcon(c("ok","ok","dismiss")), state=state.name[1:3], stringsAsFactors=FALSE)
f <- gframe("Works with single column, but can feed data frame too (size set)", cont=g)
tbl <- gtable(m, cont=f)
size(tbl) <- c(500, 100)



## [<- method
f <- gframe("can reset values to choose from", horizontal=FALSE,cont=g)
b <- gbutton("Click me to shorten to two rows", cont=f)
tbl <- gtable(d, cont=f);
size(tbl) <- c(500,200)
addHandlerClicked(b, handler=function(h,...) {
  assign("tmp", list(h, tbl,  d[1:2,]), .GlobalEnv)
  
  h$action[] <- d[1:2,]
}, action=tbl)

## ## click handler
f <- gframe("addHandlerClick for selection click, also add handler double click", cont=g)
tbl <- gtable(d, cont=f)
size(tbl) <- c(500,100)
addHandlerClicked(tbl, handler=function(h,...) {
  gmessage(sprintf("Value: %s, index: %s", svalue(h$obj, index=FALSE), svalue(h$obj, index=TRUE)), parent=h$obj)
})

## ## sorting
f <- gframe("Can sort by clicking on headers. -- Try it", cont=g)
tbl <- gtable(d, cont=f)
size(tbl) <- c(500,100)
addHandlerClicked(tbl, handler=function(h,...) {
  gmessage(sprintf("Value: %s, index: %s", svalue(h$obj, index=FALSE), svalue(h$obj, index=TRUE)), parent=h$obj)
})


## ## filtering
f <- gframe("Filtering done by  method filter.", horizontal=FALSE, cont=g)
f1 <- ggroup(cont=f)
glabel("No Cyls:", cont=f1)
cb <- gcombobox(c("<All>", sort( unique(mtcars$cyl))), cont=f1)
tbl <- gtable(mtcars[, 1:5], cont=f)
size(tbl) <- c(500,300)
addHandlerChanged(cb, handler=function(h,...) {
  val <- svalue(h$obj)
  if(val == "<All>")
    val <- ""
  h$action$filter("cyl", val)    # clears if ""
}, action=tbl)

## Paging -- XXX
f <- gframe("Example of paging for handling large data sets", cont=g)
library(MASS)
tbl <- gtable(Aids2, paging=TRUE, cont=f)
size(tbl) <- c(500,100)
