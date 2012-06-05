## workspace browser

describe <- function(x, ...) UseMethod("describe")
describe.default <- function(x, ...) sprintf("An R object of class <em>%s</em>", class(x)[1])
describe.numeric <- function(x, ...) sprintf("A numeric vector of length %s", length(x))
describe.integer <- function(x, ...) sprintf("An integer of length %s", length(x))
describe.character <- function(x, ...) sprintf("A character vector of length %s", length(x))
describe.factor <- function(x, ...) {
  l <- levels(f)
  n <- length(l)
  sprintf("A factor with levels %s%s", if(n > 5) head(l,n=5) else l, if(n > 5) "..." else "")
}
describe.matrix <- function(x, ...) sprintf("A matrix with %s rows and %s columns", nrow(x), ncol(x))
describe.data.frame <- function(x, ...) sprintf("A matrix with %s rows and %s columns", nrow(x), ncol(x))
describe.list <- function(x, ...) sprintf("A list with %s components", length(x))

##' List object in workspace
##'
##' Return data frame with objects in workspace
list_objects <- function() {
  x <- ls(envir=.GlobalEnv)
  descr <- sapply(x, function(i) describe(get(i, .GlobalEnv)))
  data.frame(Name=x, Description=descr, stringsAsFactors=FALSE)
}

##' provide detail on x
##' 
##' @param x  object to give detail on
##' @param varname variable name
##' @param cont container
##' @param width pixel width
##' @param height pixel height
##' @param ... 
detailOn <- function(x, varname, cont, width, height,...) UseMethod("detailOn")
detailOn.default <- function(x, varname, cont, width, height,...) {
  out <- capture.output(x)
  if(length(out) > 20)
    out <- c(head(out, n=20), "....")
  t <- gtext(paste(out, collapse="\\n"), cont=cont, expand=TRUE)
  size(t) <- c(width, height)
}
detailOn.numeric <- function(x, varname, cont, width, height,...) {
  l <- data.frame(x=x, stringsAsFactors=FALSE)
  names(l) <- varname
  gtable(l, cont=cont, width=width, height=height)
}
detailOn.factor <- function(x, varname, cont, width, height, ...) {
  l <- data.frame(x=x)
  names(l) <- varname
  gtable(l, cont=cont, width=width, height=height)
}
detailOn.matrix <- function(x, varname, cont, width, height,...) {
  g <- ggroup(cont=cont, horizontal=FALSE)
  gtable(as.data.frame(x), cont=g, width=width, height=height)
}
detailOn.data.frame <- function(x, varname, cont, ...) {
  detailOn.matrix(x, varname, cont, ...)
}

## detailOn.list <- function(x, varname, cont, width, height, ...) {
##   offspring <- function(path, offspring.data, ...) {
##     getInfo <- function(x) data.frame(Name=names(x),
##                                       has.offspring=sapply(names(x), function(i) is.recursive(x[[i]])),
##                                       stringsAsFactors=FALSE)
##     if(length(path) == 0)
##       getInfo(offspring.data)
##     else
##       getInfo(offspring.data[[path]])
##   }
##   t <- gtree(offspring=offspring, offspring.data=x, cont=cont)
## #  size(t) <- c(width, height)
## }

##################################################

w <- gwindow("Object browser")
gstatusbar("Object browser. Click update to refresh the listing; click an item for detail.", cont=w)

g <- gframe("Objects in your global workspace", cont=w, horizontal=FALSE)
workspace_objects <- gtable(list_objects(), cont=g,
                            expand=TRUE, fill=TRUE, handler=function(h,...) {

  val <- svalue(h$obj)
  w1 <- gwindow(sprintf("Detail on %s", val), parent=w, width=400, height=500) 
  g <- ggroup(cont=w1, horizontal=FALSE)
  detailOn(get(val, .GlobalEnv), val, g, width=400, height=450)
  gseparator(cont=g)
  bg <- ggroup(cont=g); addSpring(bg)
  gbutton("Dismiss", cont=bg, handler=function(h,...) dispose(w1))
  visible(w1) <- TRUE
})

bg <- ggroup(cont=g); 
gbutton("Update...", cont=bg, handler=function(h,...) {

  vals <- list_objects()
  workspace_objects[] <- vals
})


visible(w) <- TRUE
