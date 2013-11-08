w <- gwindow("statusbar")
sb <- gstatusbar("statusbar", cont=w)

g <- ggroup(cont=w)
sb1 <- gstatusbar("inner bar", cont=g)

## test
## svsalu
expect_equal(svalue(sb), "statusbar")

## svalue<--
svalue(sb) <- "new"
expect_equal(svalue(sb), "new")
