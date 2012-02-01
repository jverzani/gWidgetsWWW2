w <- gwindow("gdf example")
sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)

g <- ggroup(cont=w, horizontal=FALSE)
glabel("The gdf widget allows editing of cells.", cont=g)

m <- data.frame(integer=1:3, numeric=rnorm(1:3), character=state.name[1:3], factor=factor(state.name[1:3]),
                               logical=c(T,T,F),
                stringsAsFactors=FALSE)

d <- gdf(m, width="auto", cont=g)

bg <- ggroup(cont=g)
gbutton("d[1,1]", cont=bg, handler=function(h,...) {gmessage(as.character(d[1,1]), parent=w)})
gbutton("names(d)[1] <- 'new name'", cont=bg, handler=function(h,...) names(d)[1] <- "new name")
gbutton("d$set_column_width(1, 200)", cont=bg, handler=function(h,...) d$set_column_width(1, 200))
