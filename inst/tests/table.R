w <- gwindow("gtable")
g <- ggroup(cont=w, horizontal=FALSE)

x <- data.frame("key"=state.name[1:10], value=state.x77[1:10,'Population'], stringsAsFactors=FALSE)
y <- x
y$icons <- asIcon(rep("ok", length=10))

##
tbl1 <- gtable(x, cont=g)
tbl2 <- gtable(y, cont=g)


## test
## svalue
expect_equal(svalue(tbl1), NA)

## svalue<-
svalue(tbl1, index=TRUE) <- 1
expect_equal(svalue(tbl1, index=TRUE), 1)
expect_equal(svalue(tbl1, drop=TRUE), state.name[1])
