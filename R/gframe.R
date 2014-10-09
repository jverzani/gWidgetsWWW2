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

##' @include ggroup.R
NULL


##' gframe is a title-decorated ggroup box container
##'
##' Use \code{svalue<-} to adjust the title
##' @param text label text
##' @param pos position of label. Ignored?
##' @param horizontal Boolean. Set to \code{TRUE} to pack children horizontally.
##' @param spacing Integer. Between child spacing.
##' @param use.scrollwindow Boolean. Set to \code{TRUE} to add a
##' scrollwindow to manage space given to child widgets. Containers
##' with scrollwindows often have their size fixed.
##' @inheritParams gwidget
##' @return an \code{GContainer} object
##' @seealso \code{\link{ggroup}}
##' @export
##' @examples
##' w <- gwindow()
##' g <- gframe("Label", cont=w)
##' b <- gbutton("insider frame", cont=g)
##' svalue(g) <- "new label"
gframe <- function(text = "", pos = 0, horizontal=TRUE, spacing=2,
                   use.scrollwindow=FALSE,
                   container=NULL,...,
                   width=NULL, height=NULL, ext.args=NULL
                   ) {

  f <- GFrame$new(container, ...)
  f$init(horizontal=horizontal,
         spacing=spacing,
         use.scrollwindow=use.scrollwindow,
         container,
         ...,
         width=width,
         height=height, 
         ext.args = ext.args)
  f$set_value(text)
  f
}

## base class for gframe
GFrame <- setRefClass("GFrame",
                       contains="GGroup",
                       fields=list(
                         value = "ANY"
                         ),
                       methods=list(
                         get_value = function(...) {
                           value
                         },
                         set_value = function(value, ...) {
                           value <<- value
                           call_Ext("setTitle", value)
                         }
                         )
                       )
