##      Copyright (C) 2011  John Verzani
##  
##      This program is free software: you can redistribute it and/or modify
##      it under the terms of the GNU General Public License as published by
##      the Free Software Foundation, either version 3 of the License, or
##      (at your option) any later version.
##  
##      This program is distributed in the hope that it will be useful,
##      but WITHOUT ANY WARRANTY; without even the implied warranty of
##      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##      GNU General Public License for more details.
##  
##      You should have received a copy of the GNU General Public License
##      along with this program.  If not, see <http://www.gnu.org/licenses/>.

##' Browse a data frame in the global environment using gtable
##'
##' @param d data frame
##' @param width width of display
##' @param height height of display
##' @param paging Logical. Do we have paging to display values. Should use if \code{nrow(d) > 200} or so
##' @param ... ignored
##' @return creates webpage showing data frame
##' @export
##' @examples
##' \dontrun{
##' browse_df(mtcars)
##' }
browse_df <- function(d, width=800, height=500, paging=nrow(d) > 200, ...) {
  x <- paste(
             sprintf("w <- gwindow('Browse %s');", deparse(substitute(d))),
             sprintf("tbl <- gtable(%s, cont=w, expand=TRUE, paging=%s);", deparse(substitute(d)), paging),
             sprintf("size(tbl) <- c(%s, %s)", width, height),
             sep="\n")

  f <- tempfile()
  cat(x, file=f, append=FALSE)
  load_app(f, app_name="browse_df")
}
