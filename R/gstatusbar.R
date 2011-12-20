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

##' @include gwidget.R
NA

##' Status bar for gwindow instances
##'
##' Status for main window. Use \code{gwindow} instance for parent
##' container. The \code{svalue<-} method can be used to change the
##' value.
##' @param text text for label
##' @param container parent window
##' @param ... ignored
##' @return an ExtWidget instance
##' @export
##' @examples
##' w <- gwindow()
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook")
gstatusbar <- function(text = "", container=NULL, ...) {
  if(!is(container, "GWindow"))
    return()

  sb <- GStatusbar$new(container, ...)
  sb$init(text, container, ...)
  sb
}

##' base class for gstatusbar
##' @name gstatusbar-class
GStatusbar <- setRefClass("GStatusbar",
                          contains="GWidget",
                          fields=list(
                            container="ANY"
                            ),
                          methods=list(
                            init=function(text, container, ...) {
                              container <<- container
                              if(!missing(text))
                                set_value(text)
                            },
                            set_value = function(value, ...) {
                              value <<- value
                              container$set_status(value)
                            },
                            clear = function() {
                              set_value("")
                            }
                            ))
