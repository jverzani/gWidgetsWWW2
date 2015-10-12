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
NA

##' checkbox widget
##' 
##' @param text character. text label for checkbox. 
##' @param checked logical. initial state (Set later with \code{svalue<-})
##' @param use.togglebutton logical. XXX not implemented If TRUE, represent with a togglebutton, else use check box 
##' @inheritParams gwidget
##' @export
##' @note No method to set label
##' @examples
##' w <- gwindow()
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
##' cb <- gcheckbox("Check me?", cont=w, handler=function(h,...) {
##'   if(svalue(h$obj)) galert("checked", parent=w)
##' })
gcheckbox = function(text="", checked = FALSE, use.togglebutton=FALSE,
  handler = NULL, action = NULL,  container = NULL,...,
  width=NULL, height=NULL, ext.args=NULL) {

  cb <- GCheckbox$new(container$toplevel)
  cb$init(text, checked, use.togglebutton, handler, action, container, ...,
                  width=width, height=height, ext.args=ext.args)
  return(cb)
  }


##' A group of checkboxes
##' 
##' @param items vector of items to select from
##' @param checked initial value of checked state. Recycled
##' @param horizontal Layout horizontally?
##' @param use.table Needs implementing. If TRUE, uses a grid widget with checkboxes to
##' display. If TRUE, horizontal is ignored, and items may be a data
##' frame.
##' @inheritParams gwidget
##' @return A \code{GCheckboxGroup} reference class instance
##' @export
##' @examples
##' w <- gwindow()
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
##' cbg <- gcheckboxgroup(state.name[1:4], cont=w)
gcheckboxgroup = function (items, checked = FALSE, horizontal = FALSE, use.table=FALSE,
  handler = NULL, action = NULL,
  container = NULL, ...,
  width=NULL, height=NULL, ext.args=NULL) {
  
  cb <- GCheckboxGroup$new(container, ...)
  cb$init(items, checked, horizontal, use.table, handler, action, container, ...,
                     width=width, height=height, ext.args=ext.args)
  return(cb)
}



## Base class for checkbox group
## @note TODO share code with gradio -- one should be a subclass Gradio - GCheckboxGroup - GCheckbox
GCheckboxGroup <- R6Class("GCheckboxGroup",
  inherit = GWidget,
  public = list(
    items = NULL,
    init = function(items,
      checked = FALSE, horizontal=FALSE, use.table=FALSE,
      handler = NULL, action = NULL, container = NULL, ...,
      width=NULL, height=NULL, ext.args=NULL, columns=ifelse(!horizontal,1,length(items))) {
      
      self$value <- checked # value is index
      self$items <- items
      
      self$constructor <- "Ext.form.CheckboxGroup"
      self$transport_signal <- "change"
      self$change_signal <- "change"
      
      arg_list <- list(items=String(items_as_array()),
                       width = width,
                       height = height,
                       columns=columns,
                       vertical=!horizontal,
                       fieldLabel=list(...)$label
                       )
      self$add_args(arg_list)

      self$setup(container, handler, action, ext.args, ...)
      
      self$set_value(checked)
      self
    },
    ## main property. We store the index in value, not the label
    get_value = function(...) {
      "Return label"
      self$items[self$value]
    },
    set_value = function(value, ...) {
      "Set value. Value may be index, logical, or labels"
      if(is.logical(value)) {
        val <- rep(value, len=length(self$items))
        self$set_index(which(val))
      } else {
        out <- Filter(function(x) !is.na(x), match(value, self$items))
        if(length(out))
          self$set_index(out)
      }
    },
    get_index=function(...) {
      value
    },
    set_index=function(value, ...) {
      self$value <- value
      l <- list()
      l[[self$get_id()]] <- String(toJSArray(value))
      self$call_Ext("setValue", l)
    },
    get_items = function(i, ...) {
      self$items[i]
    },
    set_items = function(items, ...) {
      "Set items, update GUI"
      warning(gettext("No method to set items"))
      ## XXX How to update radio buttons?
      ## items <<- items
      ## XXX update radio buttons??? TODO
    },
    ## cost-free aliases
    get_names=function(...) self$get_items(...),
    set_names = function(...) self$set_items(...),
    ## transport, brings back index as string
    transport_fun = function() {
      tpl <- "
var x = [];
Ext.each(this.getChecked(), function(val) {
  x.push(val.inputValue);
});
var param = {value: x};
"
      tpl
    },
    process_transport = function(value) {
      ## value is a list
      value <<- as.numeric(unlist(value))
    },
    param_defn=function(signal) {
      if(signal == "change") {
        transport_fun()

     } else {
        ""
      }
    },
    ##
    items_as_array = function() {
      "Return items as array"
      makeCheck <- function(label, i,  name) {
        ## inputValue is 1-based index
        ## escape ' in label

        sprintf("new Ext.form.Checkbox({boxLabel:'%s', inputValue: %s, name:'%s'})",
                escapeSingleQuote(label), i, escapeSingleQuote(name))
      }
      buttons <- mapply(makeCheck, self$items, seq_along(self$items), 
                        rep(self$get_id(), len=length(self$items)))

      out <- paste("[",
                   paste(buttons, collapse=","),
                   "]", sep="")
      return(out)
    },
    add_handler_clicked = function(...) {
      "Click here is change -- perhaps through some method call, not just a moust event"
      self$add_handler_changed(...)
    }
  )
)
                       
                         
## Base class for gcheckbox
GCheckbox <- R6Class("GCheckbox",
  inherit = GCheckboxGroup,
  public = list(
    init=function(text, checked=FALSE,
                  use.togglebutton=FALSE,
                  handler=NULL, action=NULL, container=NULL, ...,
                  width=NULL, height=NULL, ext.args=NULL) {

      self$value <- if(checked) 1 else numeric(0)
      self$items <- text
    
      self$constructor <- "Ext.form.CheckboxGroup"
      self$transport_signal <- "change"
      self$change_signal <- "change"

      arg_list <- list(items=String(items_as_array()),
                       width = width,
                       height = height,
                       fieldLabel=list(...)$label
                       )
      self$add_args(arg_list)

      self$setup(container, handler, action, ext.args, ...)
      
      self$set_value(as.logical(rep(checked, len=length(self$items))), index=FALSE)
      self
    },
    get_value = function(...) {
      "Return logical, label via []"
      self$get_index(...)
    },
    set_value = function(value, ...) {
      "Set value. Value is logical TRUE or FALSE"
      self$set_index(value)
    },
    get_index=function(...) {
      return(1 %in% self$value)
    },
    set_index=function(value, ...) {
      if(as.logical(value)) {
        super$set_index(value=1)
      } else {
        super$set_index(value=numeric(0))
      }
    }
  )
)
