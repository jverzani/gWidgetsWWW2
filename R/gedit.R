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


##' gedit widget
##'
##' No [<- method. This can be done with a combobox though.
##' @param text initial text
##' @param width width in characters. Converted to pixels by multiplying by 8.
##' @param coerce.with Function to call for coercion from text. If no
##' coercion be careful when using the values, as the user can potentiall type in malicious things.
##' @param initial.msg initial message to user, in greyed out text
##' @inheritParams gwidget
##' @return a \code{GEdit} referece class object
##' @export
##' @examples
##' w <- gwindow()
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
##' e <- gedit("initial text", cont=w)
##' addHandlerChanged(e, handler=function(h,...) {
##' galert(paste("You entered", svalue(h$obj)), parent=w)
##' })
gedit <- function (text = "", width = 25, coerce.with = NULL, initial.msg="",
                   handler = NULL,  action = NULL, container = NULL, ...,
                   ext.args=NULL) {

  e <- GEdit$new(container, ...)
  e$init(text, width, coerce.with, initial.msg, handler, action, container, ..., ext.args=ext.args)
  return(e)
}



##' base class for gedit
##' 
##' For the \code{GEdit} class, the change signal is  "change". Use
##' \code{add_handler_enter} for enter key press and
##' \code{add_handler_blur} for focus out events
##' @rdname gWidgetsWWW2-package
GEdit <- setRefClass("GEdit",
                     contains="GWidgetText",
                     fields=list(
                       stub="ANY"
                       ),
                     methods=list(
                       init=function(text = "", width = 25, coerce.with = NULL, initial.msg="",
                         handler = NULL,  action = NULL, container = NULL, ...,
                         ext.args=NULL
                         ) {

                         initFields(
                                    value=text,
                                    coerce_with=coerce.with,
                                    constructor="Ext.form.TextField",
                                    transport_signal="keypress",
                                    change_signal="change"
                                    )
                         
                         ## constructor arguments
                         arg_list <- list(value = text,
                                          enableKeyEvents=TRUE,
                                          width = ifelse(is.character(width), width, sprintf("%spx", 8*width))#,
                                          ##emptyText=ifelse(nchar(initial.msg), initial.msg, NULL)
                                          )
                         add_args(arg_list)

                         setup(container, handler, action, ext.args, ...)
                       },
                       transport_fun = function() {
                         "var param = {value: w.getValue()}"
                       },
                       param_defn=function(signal) {
                         if(signal == change_signal) {
                           "var param = {value: newValue};"
                         } else {
                           ""
                         }
                       },
                       prepare_for_handler=function(signal, params) {
                         if(signal == change_signal) {
                           process_transport(params)
                         }
                       },
                       add_handler_enter=function(handler, action=NULL, ...) {
                         "add handler key for enter event"
                         signal <- "enterkey"
                         o <- observer(.self, handler, action) # in gWidgets2 but not now
                         add_observer(o, signal)
                         cmd <- sprintf("%s.on('specialkey', function(w,e,opts) {if(e.keyCode == e.ENTER) {callRhandler('%s', 'enterkey', Ext.JSON.encode(null))}}, null, {delay:100, buffer:100, single:false});",
                                        get_id(), get_id())
                         add_js_queue(cmd)
                       }
                       
                       ))
                     
