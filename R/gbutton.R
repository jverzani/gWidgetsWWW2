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


##' Basic button widget
##'
##' A button responds to mouse clicks by calling its handler
##'
##' @param text button text
##' @inheritParams gwidget
##' @param action passed to callback as \code{h$action}
##' @return a \code{GButton} instance
##' @export
##' @examples
##' w  <- gwindow("test")
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
##' g <- ggroup(cont=w, horizontal=FALSE)
##' b <- gbutton("click me", cont=g, handler=function(h,...) {
##'   galert("hello world", parent=w)
##' })
##' ## has an icon
##' b <- gbutton("up", cont=g)
##' ## just an icon using ext override to remove text but leave icon
##' b <- gbutton("help", cont=g); b$call_Ext("setText")
##' ## an action
##' a <- gaction("help", parent=w, handler=function(h,...) {
##'  galert("action", parent=w)
##' })
##' b <- gbutton(action=a, cont=g)
gbutton <- function(text="",
                    handler=NULL,
                    action=NULL,
                    container=NULL, ...,
                    width=NULL,
                    height=NULL,
                    ext.args=NULL
                    ) {
  if(!is.null(action) && is(action,"GAction")) {
    ## with an action
    b <- GButtonWithAction$new(container, ...)
    b$init(action, container, ...,  width=width, height=height, ext.args=ext.args)
  }  else {
    ## regular button
    b <- GButton$new(container, ...)
    b$init(text,  handler, action, container, ..., width=width, height=height, ext.args=ext.args)
  }
  return(b)
}

GButton <- R6Class("GButton",
  inherit = GWidget,
  public = list(
    init = function(
                    text="",
                    handler=NULL, action=NULL, container, ...,
                    width=NULL, height=NULL, ext.args=NULL
                    ) {
      self$value = text
      self$constructor = "Ext.Button"
      self$change_signal = "click"

      arg_list <- list(
                       width = width,
                       height = height
                       )

      self$add_args(arg_list)
      
      self$setup(container, handler, action, ext.args, ...)

      self$set_value(text)
    },
                        
    ## main property
    get_value = function(...) {
      "Return label"
      value
    },
    
    set_value = function(text, ...) {
      "Set label"
      self$value <- text
      self$call_Ext("setText", text)
      u <- getStockIconByName(text)
      self$call_Ext("setIconCls", getWithDefault(u, ""))
    },
    
    ##
    set_icon = function(icon) {
      "Set icon"
      self$call_Ext("setIcon", icon)
    },
    set_tooltip = function(tip) {
      self$call_Ext("setTooltip", tip)
    },
    add_handler_clicked=function(handler, action=NULL, ...) {
      self$add_handler_changed(handler, action, ...)
    }
  )
)
                       

GButtonWithAction <- R6Class("GButtonWithAction",
  inherit = GButton,
  public = list(
    init = function(action, container,  ...,  width, height, ext.args) {
      "Initialize widget if it is an action item"
      ## XXX steal ext.args .. from setup
      cmd <- sprintf("var %s = new Ext.Button(%s)", self$get_id(), action$get_id())
      self$add_js_queue(cmd)
      container$add(self, ...)
    }
  )
)
