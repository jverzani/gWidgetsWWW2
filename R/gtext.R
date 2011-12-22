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

##' @include gwidget-text.R
NULL

##' A text area widget
##'
##' @param text initial text
##' @param width width in pixels
##' @param height height in pixels
##' @param font.attr Ignored. Default font attributes
##' @param wrap Ignored Do we wrap the tet
##' @inheritParams gwidget
##' @param resizable Ignored. (Should area be resizable on the page)
##' @return an ExtWidget instance
##' @export
##' @examples
##' w <- gwindow("gtext example")
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
##' g <- ggroup(cont=w, horizontal=FALSE)
##' t <- gtext("Some text with \n new lines", cont=g)
##' b <- gbutton("click", cont=g, handler=function(h,...) {
##'   galert(svalue(b), parent=w)
##' })
##'  b <- gbutton("change", cont=g, handler=function(h,...) {
##'    svalue(t) <= "some new text"
##' })
gtext <- function(text = NULL, width = NULL, height = 300,
                  font.attr = NULL, wrap = TRUE,
                  handler = NULL, action = NULL, container = NULL,...,
                  ext.args=NULL,
                  resizable = FALSE     # gWidgetsWWW arg. Keep?
                  ) {

  txt <- GText$new(container,...)
  txt$init(text, font.attr, wrap, handler, action, container, ...,
           width=width, height=height,
           ext.args=ext.args, resizable=resizable)
  txt
}

## TODO: ace editing? 
##       fonts

##' base class for gtext
##' @name gtext-class
GText <- setRefClass("GText",
                     contains="GWidgetText",
                     method=list(
                       init = function(text, font.attr, wrap, handler, action, container, ...,
                         width, height, 
                         ext.args=ext.args, resizable=resizable) {

                         value <<- getWithDefault(text, "")
                         constructor <<- "Ext.form.TextArea"

                         transport_signal <<- "change"
                         
                         arg_list <- list(value=String(ourQuote(.self$value)),
                                          width=width,
                                          height=height,
                                          selectOnFocus = TRUE,
                                          enableKeyEvents=TRUE)

                         add_args(arg_list)
                         setup(container, handler, action, ext.args, ...)
                       },
                       insert = function(value, where, do.newline=TRUE) {
                         "Insert new text at ..."

                         ## we don't have an insert method so we add and change
                         if(where == "beginning")
                           newValue <- paste(value,
                                             .self$value,
                                             sep=ifelse(do.newline, "\n", ""))
                         else
                           newValue <- paste(.self$value,
                                             value,
                                             sep=ifelse(do.newline, "\n", ""))
                         value <<- newValue

                         call_Ext("setRawValue", value)

                       }
                       ))
                                          
