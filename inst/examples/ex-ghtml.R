w <- gwindow("Test of ghtml widget")
gstatusbar("Powered by gWidgetsWWW2 and Rook", cont = w)

require(hwriter)

g <- ggroup(cont =w, horizontal=FALSE)

g1 <- gframe("Text with HTML markup", cont = g, horizontal=FALSE)
ghtml("test", cont = g1)
ghtml("<b>test</b>", cont = g1)

g1 <- gframe("HTML from hwriter: hmakeTag and hwrite", cont = g, horizontal=FALSE)
out <- hmakeTag("h3", 'hmakeTag')
ghtml(out, cont = g1)

out <- gsub("\n","", hwrite(mtcars[1:3, 1:4]))
ghtml(out, cont = g1)


## interactive
g1 <- gframe("Using a handler to change the displayed text", cont = g, horizontal=FALSE)
mpg <- mtcars$mpg
g2 <- ggroup(cont = g1)
glabel("Cars with miles per gallon greater than:  ", cont = g2)
mpgLabel <- glabel(as.character(floor(min(mpg))), cont=g2)


sl <- gslider(min(mpg), max(mpg), by=1, cont = g2, handler = function(h,...) {
  curValue <- svalue(sl)
  svalue(mpgLabel) <- curValue
  out <- gsub("\n","", hwrite(mtcars[mpg >= as.numeric(curValue), ]))
  svalue(tbl) <- out
})

out <- gsub("\n","", hwrite(mtcars[mpg >= 0, ]))
tbl <- ghtml(out, cont = g1)




