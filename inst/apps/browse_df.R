library(gWidgetsWWW2)

##' Browse a data frame in the global environment using gtable
##'
##' @param d data frame
##' @param width width of display
##' @param height height of display
##' @param paging. Logical. Do we have paging to display values. Should use if \code{nrow(d) > 200} or so
##' @return creates webpage showing data frame
##' @export
##' @examples
##' browse_df(mtcars)
browse_df <- function(d, width=800, height=500, paging=TRUE, ...) {
  x <- paste(
             sprintf("w <- gwindow('Browse %s');", deparse(substitute(d))),
             sprintf("tbl <- gtable(%s, cont=w, expand=TRUE, paging=%s);", deparse(substitute(d)), paging),
             sprintf("size(tbl) <- c(%s, %s)", width, height),
             "visible(w) <- TRUE;",
             sep="\n")

  f <- tempfile()
  cat(x, file=f, append=FALSE)
  load_app(f)
}
 
