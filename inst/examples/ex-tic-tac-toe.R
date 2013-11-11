## A response to a stackoverflow question. A tic-tac-toe game where
## each click alternates "x" and "o".

w <- gwindow("Tic-Tac-Toe")
glabel("This example requires the <em>canvas</em> package, which is no longer available. Until it returns, this is deprected", cont=w)


## w <- gwindow("Tic-Tac-Toe")
## sb <- gstatusbar("Powered by gWidgetsWWW2 and Rook", cont=w)
## g <- gvbox(cont=w)
## ghtml("Tic-Tac-Toe, three in a row", cont=g)


## ## sizing
## margin <- 10
## sz <- 130
## width <- height <- 3*sz + 2 * margin

## state <- matrix(character(9), nrow=3)
## x_move <- TRUE
## sym <- c("o", "x")


## cnv <- gcanvas(width=width, height=height, cont=g)

## ##' make the game board
## make_board <- function() {
##   for(i in seq(margin + sz, margin + 2*sz, by=sz)) {
##     cnv$lines(c(margin, width-margin), c(i, i))
##     cnv$lines(c(i, i), c(margin, width-margin))
##   }
## }

## ##' function to draw an x in cell i,j
## draw_x <- function(i, j) {
##   delta <- sz/10
##   cnv$lines(margin + sz *c(i-1, i) + delta*c(1,-1),
##             margin + sz *c(j-1, j) + delta*c(1,-1))
##   cnv$lines(margin + sz *c(i-1, i) + delta*c(1,-1),
##             margin + sz *c(j, j-1) + delta*c(-1,1))

## }

## ##' function to draw an "o" in cell i,j
## draw_o <- function(i, j) {
##   cnv$circle(margin + (i-1/2)*sz, margin + (j-1/2)*sz, r = (3/8)*sz)
## }

## ##' what to do when there is a winner
## notify_winner <- function(mark) {
##   if(mark == "x") {
##     svalue(sb) <- "Congrats, x won"
##   } else {
##     svalue(sb) <- "Congrats, o won"
##   }

## }

## ##' check if there is a winner; if so notify, remove handler return TRUE; else return FALSE
## check_winner <- function() {
##   ## x
##   is_winner <- function(mark) {
##     any(colSums(state == mark) == 3)  ||
##     any(rowSums(state == mark) == 3) ||
##     sum((state == mark)[c(1,5,9)]) == 3 ||
##     sum((state == mark)[c(3,5,7)]) == 3
##   }
##   if(is_winner("x")) {
##     notify_winner("x")
##     removeHandler(cnv, cbid)
##     return(TRUE)
##   } else if(is_winner("o")) {
##     notify_winner("o")
##     removeHandler(cnv, cbid)    
##     return(TRUE)
##   }
##   return(FALSE)
## }


## ##' What to do when a cell is clicked
## cbid <- addHandlerClicked(cnv, handler=function(h,...) {
##   ## X,Y components in "pixel" coordinates in [0,width] x [0, height]; with (0,0) in upper left
##   i <- 1 + (h$X - margin) %/% sz
##   j <- 1 + (h$Y - margin) %/% sz

##   if(i %in% 1:3 && j %in% 1:3) {
##     if(state[i,j] == "") {
##       state[i,j] <<- sym[1 + x_move]
##       if(x_move) {
##         draw_x(i,j)
##       } else {
##         draw_o(i,j)
##       }
##       x_move <<- !x_move
##       if(!check_winner())
##         svalue(sb) <- "Click to move"
##     } else {
##       svalue(sb) <- "Already selected that cell"
##     }
##   }

## })


## ## Start
## make_board()
## svalue(sb) <- "Click to  move"
