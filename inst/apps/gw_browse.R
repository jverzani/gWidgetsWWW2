## workspace browser

describe <- function(x, ...) UseMethod("describe")
describe.default <- function(x, ...) sprintf("An R object of class %s", class(x)[1])
describe.numeric <- function(x, ...) sprintf("A numeric vector of length %s", length(x))
describe.integer <- function(x, ...) sprintf("A integer of length %s", length(x))
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
  if(length(out) > 10)
    out <- c(head(out, n=10), "....")
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
detailOn.matrix <- function(x, varname, cont, ...) {
  g <- ggroup(cont=cont, horizontal=FALSE)
  glabel(sprintf("Detail on %s", varname), cont=cont)
  gtable(as.data.frame(x), cont=g, width=width, height=height)
}
detailOn.data.frame <- function(x, varname, cont, ...) {
  detailOn.matrix(x, varname, cont, ...)
}
detailOn.list <- function(x, varname, cont, width, height, ...) {
  offspring <- function(path, offspring.data, ...) {
    getInfo <- function(x) data.frame(Name=names(x),
                                      has.offspring=sapply(names(x), function(i) is.recursive(x[[i]])),
                                      stringsAsFactors=FALSE)
    if(length(path) == 0)
      getInfo(offspring.data)
    else
      getInfo(offspring.data[[path]])
  }
  t <- gtree(offspring=offspring, offspring.data=x, cont=cont)
#  size(t) <- c(width, height)
}

##################################################

w <- gwindow("Object browser")
w1 <- NULL                              # subwindow with detail
g <- gframe("Objects in workspace", cont=w, horizontal=FALSE)
gbutton("Update...", cont=g, handler=function(h,...) {
  if(!is.null(w1))
    visible(w1) <- FALSE
  vals <- list_objects()
  workspace_objects[] <- vals
})
workspace_objects <- gtable(list_objects(), cont=g,
                            width=800, height=400, handler=function(h,...) {
  if(!is.null(w1))
    visible(w1) <- FALSE
  val <- svalue(h$obj)
  w1 <- gwindow(sprintf("Detail on %s", val), parent=w, width=400, height=500)
  g <- ggroup(cont=w1, horizontal=FALSE)
  detailOn(get(val, .GlobalEnv), val, g, width=400, height=400)
  gseparator(cont=g)
  gbutton("Dismiss", cont=g, handler=function(h,...) dispose(w1))
  visible(w1) <- TRUE
})

gstatusbar("Object browser. Click update to refresh list. Double click for detail.", cont=w)
visible(w) <- TRUE
