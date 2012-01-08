## example to show all the buttons with their icons.

w <- gwindow("Example of icons with buttons")
sb <- gstatusbar("Powered by Rook and gWidgetsWWW2", cont = w)

x <- getStockIcons()
g <- gframe("First 10 buttons with stock icons in gWidgetsWWW", horizontal=FALSE, cont=w)
for(i in names(x)[1:10]) {
  g2 <- ggroup(cont=g)
  gbutton(i, cont=g2)
}

