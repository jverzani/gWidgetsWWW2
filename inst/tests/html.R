w <- gwindow("ghtml")
g <- ggroup(cont=w, horizontal=FALSE)

h1 <- ghtml("no markup", cont=g, fill="x")
h2 <- ghtml("<i>emphasize</i>", cont=g)
## from url
f <- get_tempfile(ext=".html")
cat("<i>this is in a file</i>", file=f)

h3 <- ghtml(f, cont=g, width=300)

## test
expect_that(svalue(h1), "no markup")
expect_that(svalue(h3), f)

svalue(h2) <- "new text"
expect_that(svalue(h2), "new text")

cat("<i>replace file text</i>", file=f)
svalue(h3) <- f
expect_that(svalue(h3), f)

