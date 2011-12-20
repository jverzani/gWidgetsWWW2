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
NULL

##' A label widget
##' 
##' @param text text for label. Main property. Use \code{svalue<-} to change. 
##' @param markup logical. Ignored, but see example for \code{ext.args} usage
##' @param editable logical. Ignored
##' @param handler ignored
##' @param action ingnored
##' @param container parent container
##' @param ... passed to parent container's \code{add} method
##' @param width ignored
##' @param height ignored
##' @param ext.args additional options passed to Ext constructor.
##' @return an ExtWidget object
##' @export
##' @examples
##' w <- gwindow()
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
##' glabel("A label widget", cont=w)
##' glabel("A red label", cont=w,  ext.args=list(style=list("color"="red")))
glabel <- function(text = "", markup = FALSE, editable = FALSE,
                   handler = NULL, action = NULL, container = NULL,...,
                   width=NULL, height=NULL, ext.args=NULL
                   ) {

  l <- GLabel$new(container, ...)
  l$init(text, markup, editable, handler, action, container, ...,
         width=width, height=height, ext.args=ext.args)
  l
}

##' base class for glabel
##' @name glabel-class
GLabel <- setRefClass("GLabel",
                      contains="GWidget",
                      method=list(
                        init = function(text = "", markup = FALSE, editable = FALSE,
                          handler = NULL, action = NULL, container = NULL,...,
                          width=NULL, height=NULL, ext.args=NULL) {

                          value <<- text
                          constructor <<- "Ext.form.Label"
                          arg_list <- list(
                                           html=text
                                           )
                          
                           add_args(arg_list)

                          setup(container, handler, action, ext.args, ...)

                           .self
                        },
                        set_value = function(value, ...) {
                          value <<- value
                          call_Ext("setText", value)
                          parent$do_layout()
                        }
                        ))
