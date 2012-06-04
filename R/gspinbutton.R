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

##' Basic spinbutton
##'
##' @param from from value
##' @param to to
##' @param by by. From to by are same as seq() usage
##' @param value initial value
##' @inheritParams gwidget
##' @return an GSpinbutton reference class instance
##' @export
##' @examples
##' w <- gwindow()
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
##' sp <- gspinbutton(cont=w)
gspinbutton <- function(from = 0, to = 100, by = 1, value = from,
                        handler = NULL, action = NULL, container = NULL, ...,
                        width=NULL, height=NULL, ext.args=NULL
                        ) {

  sp <- GSpinbutton$new(container, ...)
  sp$init(from, to, by, value,  handler, action, container, ...,
          width=width, height=height, ext.args=ext.args)
  sp
}

## base class for gspinbutton
GSpinbutton <- setRefClass("GSpinbutton", 
                       contains="GWidget",
                       fields=list(
                         stub="ANY"
                         ),
                       methods=list(
                         init=function(from, to, by, value,  handler, action, container, ...,
                           coerce.with=as.numeric,
                           width, height, ext.args) {


                           initFields(
#                                      value=value,
                                      constructor="Ext.ux.CustomSpinner",
                                      transport_signal="change",
                                      change_signal="change"
                                      )

                           arg_list =list(
                             minValue=from,
                             maxValue=to,
                             step=by,
                             value=value,
                             accelerate=TRUE,
                             fieldLabel=list(...)$label
                             )
                           add_args(arg_list)

                           setup(container, handler, action, ext.args, ...)

                           set_value(value)
                           .self

                         },
                         set_value = function(value, ...) {
                           value <<- value                           

                           call_Ext("setValue", value)
                         },
                         get_items = function(...) items,
                         set_items = function(items, ...) {
                           ## XXX No methods in extjs to set the values (minValue, maxValue, increment)
                           ### after construction so we can't implement [<- method
                         },
                         transport_fun = function() {
                           "param={value: newValue}"
                         },
                         process_transport = function(value) {
                           value <<- as.numeric(value)
                         }
                         )
                       )
