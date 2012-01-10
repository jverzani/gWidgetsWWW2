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

##' calendar widget
##'
##' Basic text box with button to open calendar picker dialog. The
##' svalue method refers to the date, which depends on the value of
##' \code{format}.
##' @param text optional inital date as text.
##' @param format format of date. Default of Y-m-d.
##' @inheritParams gwidget
##' @return a \code{GCalendar} instance
##' @note the \code{svalue} method returns an instance of \code{Date}
##' class by conversion through \code{as.Date}.
##' @export
##' @examples
##' w <- gwindow("Calendar")
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
##' a <- gcalendar(cont=w)
gcalendar <- function(text = "", format = NULL,
                      handler=NULL, action=NULL, container = NULL, ...,
                      width=NULL, height=NULL, ext.args=NULL
                      ) {

  cal <- GCalendar$new(container, ...)
  cal$init(text, format, handler, action, container, ...,
           width=width, height=height, ext.arg=ext.args)
  cal
}

## Calendar class
GCalendar <- setRefClass("GCalendar",
                         contains="GWidget",
                         fields=list(
                           date_format="character"
                           ),
                         methods=list(
                           init = function(text, format, handler, action, container,
                             ...,
                             width=NULL, height=NULL, ext.args=NULL) {
                             
                             date_format <<- getWithDefault(format, "%Y-%m-%d")
                             ext_format <- gsub("%","", date_format)

                             
                             ## Ext format has no %
                             fmt <- if(!is.null(format) && nchar(format) > 0) {
                               gsub("%","",format)
                             } else {
                               NULL
                             }
                             constructor <<- "Ext.form.field.Date"
                             transport_signal <<- "change"
                             arg_list <- list(editable=TRUE,
                                              width=width,
                                              height=height,
                                              format=ext_format
                                              )
                             add_args(arg_list)

                             setup(container, handler, action, ext.args, ...)

                             if(nchar(text))
                               set_value(text)
                             else
                               set_value(NA)
                             .self
                           },
                           get_value = function(...) {
                             as.Date(value, date_format)
                           },
                           set_value = function(value, ...) {
                             value <<- value
                             if(!is.null(date_format) && nchar(date_format))
                               call_Ext("setValue", as.character(as.Date(value, date_format))) # right format?
                             else
                               call_Ext("setValue", value)
                           },
                           transport_fun = function() {
                             "param = {value: newValue}"
                           }
                           ))

                             
