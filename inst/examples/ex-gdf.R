w <- gwindow("gdf example")
sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)

g <- ggroup(cont=w, horizontal=FALSE)
glabel("The gdf widget allows editing of cells. No methods to enlarge or shrink data frame are available", cont=g)
b <- gbutton("Change names, tooltip, width and editable on column 1", cont=g, handler=function(h,...) {
  ## Some methods
  names(d)[1] <- "new name"
  d$set_column_tooltip("he htere", 1)
  d$set_column_width(25, 1)
  editable(d, column=1) <- FALSE
})

m <- data.frame(integer=1:3, numeric=rnorm(1:3), character=state.name[1:3], factor=factor(state.name[1:3]),
                               logical=c(T,T,F),
                stringsAsFactors=FALSE)

d <- gdf(m, width="auto", cont=g)
