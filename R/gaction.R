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
##'
##' See the method \code{add_keybinding} to add a simple keybinding to
##' initiate this action.
##' @param label Text for action
##' @param tooltip tooltip. Ignored for this toolkit
##' @param icon action icon class
##' @param key.accel keyboard accelerator. Single key, e.g. "X",
##' "LEFT" (arrow), "PAGE_UP", "Ctrl-n", "Alt-X". Use "Shift" to force
##' that. List of key events is given here:
##' \code{http://docs.sencha.com/ext-js/4-1/#!/api/Ext.EventObject}.
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
gaction <- function(label, tooltip=label, icon=NULL, key.accel=NULL,
                    handler, parent, ...) {
  a <- GAction$new(parent=parent)
  a$init(label, tooltip, icon, key.accel, handler, parent, ...)
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
                         init = function(label, tooltip=label, icon=NULL,
                           key.accel, handler, parent, ...) {

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

                           if(!is.null(key.accel))
                             add_keybinding(key.accel)


                                                      
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
                         },
                         add_keybinding=function(key) {
                           "Add keybinding to document for this action. Key is value for Ext.EventObject: http://docs.sencha.com/ext-js/4-1/#!/api/Ext.EventObject. Use Ctrl-X, Alt-X of Shift-X indicate keys"
                           tpl <- "
var map = new Ext.util.KeyMap(document, {
    key: Ext.EventObject.{{key}},
    handler: function() {callRhandler('{{id}}', 'action', null)},
    shift: {{shift}},
    control: {{control}},
    alt: {{alt}}
});
"
                           add_js_queue(whisker.render(tpl,
                                                       list(id=get_id(),
                                                            shift=ifelse(grepl("Shift", key), "true", "false"),
                                                            control=ifelse(grepl("Ctrl", key), "true", "false"),
                                                            alt=ifelse(grepl("Alt", key), "true", "false"),
                                                            key=toupper(tail(strsplit(key,"-")[[1]], n=1))

                                                            )))
                         }
                         ))
                         
