##      Copyright (C) 2011  John Verzani
##      Copyright (C) 2015  Johannes Ranke (port to R6)
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
GCalendar <- R6Class("GCalendar",
  inherit = GWidget,
  public = list(
    date_format = NULL,
    init = function(text, format, handler, action, container,
      ..., width=NULL, height=NULL, ext.args=NULL) {
      
      self$date_format <- getWithDefault(format, "%Y-%m-%d")

      ## Ext format has no % 
      ext_format <- gsub("%","", date_format)
      
      # JR: fmt is not used, could be removed after testing
      fmt <- if(!is.null(format) && nchar(format) > 0) {
        gsub("%","",format)
      } else {
        NULL
      }
      self$constructor <- "Ext.form.field.Date"
      self$transport_signal <- "change"
      arg_list <- list(editable=TRUE,
                       width=width,
                       height=height,
                       format=ext_format
                       )
      self$add_args(arg_list)

      self$setup(container, handler, action, ext.args, ...)

      if(nchar(text))
        self$set_value(text)
      else
        self$set_value(NA)
      self
    },
    get_value = function(...) {
      as.Date(self$value, self$date_format)
    },
    set_value = function(value, ...) {
      self$value <- value
      if(!is.null(self$date_format) && nchar(self$date_format))
        self$call_Ext("setValue", as.character(as.Date(value, date_format))) # right format?
      else
        self$call_Ext("setValue", value)
    },
    transport_fun = function() {
      "param = {value: newValue}"
    }
  )
)
