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
##' svalue method refers to the date, which depends on the value of format.xf
##' @param text optional inital date as text.
##' @param format format of date. Default of Y-m-d.
##' @param handler handler called when date changed
##' @param action action passed to handler
##' @param container parent container
##' @param ... passed to \code{add} method of container
##' @param width width of widget in pixels
##' @param height height of widget in pixels
##' @param ext.args extra arguments passed to Ext constructor
##' @return a \code{ExtWidget} instance
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

##' class for gcalendar
##' @name gcalendar-class
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
                                              height=height
                                              )
                             add_args(arg_list)

                             setup(container, handler, action, ext.args, ...)

                             if(nchar(text))
                               set_value(text)
                             
                             .self
                           },
                           get_value = function(...) {
                             as.Date(value, date_format)
                           },
                           set_value = function(value, ...) {
                             value <<- value
                             if(!is.null(date.format) && nchar(date.format))
                               call_Ext("setValue", as.Date(value, date.format)) # right format?
                             else
                               call_Ext("setValue", value)
                           },
                           transport_fun = function() {
                             "param = {value: newValue}"
                           }
                           ))

                             
