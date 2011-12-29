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

##' gaction implementation
##'
##' actions are reusable encapsulations of actions. The
##' \code{svalue<-} method can be used to update the text associated
##' with an action. Use \code{enabled<-} to adjust sensitivity to
##' mouse events. The \code{visible<-} method can adjust if objects
##' proxying the action are visible. The \code{set_icon} reference
##' class method can be used to set the icon (no S3 method)
##' @param label Text for action
##' @param tooltip tooltip. Ignored for this toolkit
##' @param icon action icon class
##' @param handler function called when action is invoked
##' @param parent toplevel window of action (where it can be
##' called). Required here
##' @param ... ignored
##' @return a \code{GWidget} object
##' @export
##' @examples
##' w <- gwindow()
##' a <- gaction("some action", handler=function(h,...) galert("asdf", parent=w), parent=w)
##' b <- gbutton(action=a, cont=w)
##' enabled(a) <- FALSE
##' svalue(a) <- "new text"
##' #
gaction <- function(label, tooltip=label, icon=NULL, handler, parent, ...) {
  a <- GAction$new(parent=parent)
  a$init(label, tooltip, icon, handler, parent, ...)
  a
}

##' Class for gaction
##' @rdname gaction-class
GAction <- setRefClass("GAction",
                       contains="GWidget",
                       fields=list(
                         handler_id = "list"
                         ),
                       method=list(
                         init = function(label, tooltip=label, icon=NULL, handler, parent, ...) {

                           add_handler("action", handler)
                           value <<- label

                           ## match with gcomponent
                           constructor <<- "Ext.Action"

                           fn <- sprintf("function() {callRhandler('%s', 'action', '')}",
                                         get_id())
                           
                           arg_list <- list(
                                            text=label,
                                            handler=String(sprintf("function() {callRhandler('%s', 'action', '')}",get_id())),
                                            iconCls=getStockIconByName(icon)
                                            )
                           
                           add_args(arg_list)
                           write_constructor()


                                                      
                         },
                         ## override this, done through handler argument at construction
                         connect_to_toolkit_signal=function(...) {},
                         get_value = function() value,
                         set_value = function(value, ...) {
                           value <<- as.logical(value)
                           call_Ext("setText", value)
                         },
                         set_visible = function(value) {
                           "Set whether widgets proxying action are visible"
                           ..visible <<- as.logical(value)
                           callExt("setHidden", as.logical(value))
                         }
                         ))
                         
