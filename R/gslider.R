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

##' slider widget
##'
##' Use slider to select from a sequence of values, specified as with
##' \code{seq}. The sequence steps must be greater than 1
##' @param from starting point. Unlike other implementations for
##' gWidgets, this is not possibly a vector specifying a range of
##' values to slide over.
##' @param to ending point
##' @param by step size. Must be larger than 1 and even then will round to integer value
##' @param value initial value
##' @param horizontal orientation
##' @inheritParams gwidget
##' @param tpl Template for tooltip. Should have "\code{{0}}" to replace the value, but can have more formatting
##' @return a \code{GSlider} reference class object
##' @export
##' @examples
##' w <- gwindow()
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
##' sl <- gslider(from=0, to=100, by=1, value=50, cont=w, tpl="{0}percent")
##' addHandlerChanged(sl, handler=function(h,...) {
##'  galert(paste("You selected", svalue(h$obj)), parent=w)
##' })
gslider <- function(from = 0, to = 100, by = 1, value = from,
                    horizontal = TRUE,
                    handler = NULL, action = NULL, container = NULL, ...,
                    width=NULL, height=NULL, ext.args=NULL,
                    tpl = "{0}"
                    ) {

  sl <- GSlider$new(container, ...)
  sl$init(from, to, by, value, horizontal, handler, action, container, ...,
          width=width, height=height, ext.args=ext.args, tpl=tpl)
  sl
}

##' \code{GSlider} is the base class for gslider
##'
##' The \code{GSlider} implementation allows one to adjust the
##' formating of the tooltip that indicates the current value of the
##' widget. The \code{tpl} command uses an HTML snippet where the
##' value \code{{0}}
##' does the substitution.
##' @rdname gslider
GSlider <- setRefClass("GSlider", 
                       contains="GWidget",
                       fields=list(
                         stub = "ANY"
                         ),
                       methods=list(
                         init=function(from, to, by, value, horizontal, handler, action, container, ...,
                           coerce.with=as.numeric,
                           width=NULL, height=NULL, ext.args=NULL, tpl="{0}") {
                           value <<- value

                           constructor <<- "Ext.slider.Single"
                           transport_signal <<- "change"
                           change_signal <<- "change"
                           coerce_with <<- coerce.with

                           ## template for slider
                           template <- paste("new Ext.slider.Tip({",
                                             "  getText: function(thumb){",
                                             sprintf("return String.format('%s', thumb.value)", escapeSingleQuote(tpl)),
                                             "}})",
                                             sep="")

                           tipText <- sprintf("function(thumb) {return Ext.String.format('%s', thumb.value);}",
                                              tpl)
                           
                           arg_list <- list(value=value,
                                            minValue=from,
                                            maxValue=to,
                                            increment=by,
                                            vertical=!horizontal,
                                            useTips=TRUE,
                                            tipText=String(tipText),
                                            enableKeyEvents=TRUE,
                                            width = width,
                                            height = height,
                                            fieldLabel=list(...)$label
                                            )
                           
                           add_args(arg_list)

                           setup(container, handler, action, ext.args, ...)
                           
                           .self

                         },
                         set_value = function(value, ...) {
                           value <<- value
                           call_Ext("setValue", value)
                         },
                         transport_fun = function() {
                           "param={value: newValue}"
                         },
                         process_transport = function(value) {
                           value <<- value
                         }                        
                         )
                       )
