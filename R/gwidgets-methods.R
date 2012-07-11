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

## Methods for gWidgets objects

##' @include gcomponent.R
##' @include gwidget.R
##' @include gcontainer.R
NULL

##################################################
## Component Methods

##' Is widget enabled, sensitive to user input
##'
##' @param x the widget
##' @export
"enabled" <- function(x) UseMethod("enabled")

##' enabled method
##' @param x the widget
##' @method enabled GComponent
##' @S3method enabled GComponent
enabled.GComponent <- function(x) x$enabled()

##' Set if widget is enabled
##'
##' @param x widget
##' @param value logical
##' @export 
##' @rdname enabled_assign
"enabled<-" <- function(x, value) UseMethod("enabled<-")

##' assignment method for enabled
##' @method enabled<- GComponent
##' @S3method enabled<- GComponent
##' @rdname enabled_assign
"enabled<-.GComponent" <- function(x, value) {
  x$set_enabled(value)
  x
}


##' Set font properties, if implemented
##'
##' @param x widget
##' @param value font specification, a named list, eg. list(size=10, color="red")
##' @export 
##' @rdname font_assign
"font<-" <- function(x, value) UseMethod("font<-")

##' assignment method for font
##' @method font<- GComponent
##' @S3method font<- GComponent
##' @rdname font_assign
"font<-.GComponent" <- function(x, value) {
  x$set_font(value)
  x
}


##' Set size property, if implemented
##'
##' @param x widget
##' @param ... passed on 
##' @param value size specification, for most widgets a pair c(width, height), but can have exceptions
##' @export 
##' @rdname size_assign
"size<-" <- function(x, ..., value) UseMethod("size<-")

##' assignment method for size
##' @method size<- GComponent
##' @S3method size<- GComponent
##' @rdname size_assign
"size<-.GComponent" <- function(x, ..., value) {
  x$set_size(value, ...)
  x
}


##' Retreive a persistent attribute for an object
##'
##' @param x the object
##' @param key key holding attribute
##' @export
tag <- function(x, key) UseMethod("tag")

##' method for tag
##' @param x the object
##' @param key key holding attribute
##' @method tag GComponent
##' @S3method tag GComponent
tag.GComponent <- function(x, key) x$get_attr(key)

##' Set a persistent attirbute for an object
##'
##' @param x object
##' @param key key to store the value
##' @param value attribute value
##' @export 
##' @rdname tag_assign
"tag<-" <- function(x, key, value) UseMethod("tag<-")

##' assignment method for tag
##' @method tag<- GComponent
##' @S3method tag<- GComponent
##' @rdname tag_assign
"tag<-.GComponent" <- function(x, key, value) x$set_attr(key, value)


##' Generically returns if widget is in visible state
##'
##' @param x object Calls objects \code{set_visible} method
##' @export 
"visible" <- function(x) UseMethod("visible")

##' getters method for visible
##'
##' @rdname visible
##' @method visible GComponent
##' @S3method visible GComponent
"visible.GComponent" <- function(x) {
  x$get_visible()
}

##' Primarily used to set if widget is shown, but also has other meanings
##'
##' @param x object Calls objects \code{set_visible} method
##' @param value logical
##' @export 
##' @rdname visible_assign
"visible<-" <- function(x, value) UseMethod("visible<-")

##' assignment method for visible
##'
##' @method visible<- GComponent
##' @S3method visible<- GComponent
##' @rdname visible_assign
"visible<-.GComponent" <- function(x,  value) {
  x$set_visible(value)
  x
}


##' Return main value associated with a widget
##'
##' @param x the widget
##' @param index if specified as \code{TRUE} calls \code{get_index}, else
##' @param drop passed along, in many cases used like \code{drop} call for \code{[}.
##' \code{get_value} reference methods.
##' @param ... passed to \code{get_value} or \code{get_index}
##' method. May include arguments \code{index} or \code{drop}
##' @export
"svalue" <- function(x, index=NULL, drop=NULL,...) UseMethod("svalue")

##' svalue method
##' 
##' @rdname svalue
##' @method svalue GComponent
##' @S3method svalue GComponent
"svalue.GComponent" <- function(x, index=NULL, drop=NULL, ...) {
  if(is.logical(index) && index)
    x$get_index(drop=drop, ...)
  else
    x$get_value(drop=drop, ...)
}

##' Set main value associated with a widget
##'
##' @param x object
##' @param index if non-NULL and \code{TRUE} call \code{set_index}
##' else call \code{set_value} reference class method.
##' @param ... passed to \code{set_value} method. May include arguments for \code{index}
##' @param value value to set
##' @export 
##' @rdname svalue_assign
"svalue<-" <- function(x, index=NULL, ..., value) UseMethod("svalue<-")

##' assignment method for svalue
##' @method svalue<- GComponent
##' @S3method svalue<- GComponent
##' @rdname svalue_assign
"svalue<-.GComponent" <- function(x, index=NULL, ..., value) {
  if(!is.null(index) && index)
    x$set_index(value, ...)
  else
    x$set_value(value, ...)
  x
}


##' Set focus onto object. 
##'
##' For some widgets, this sets user focus (e.g. gedit gets focus for
##' typing).
##' @param x object
##' @param value logical. Set focus state.
##' @export
##' @rdname focus
"focus<-" <- function(x, value) UseMethod("focus<-")

##' Basic S3 method for focus
##'
##' @export
##' @rdname focus
##' @method focus<- default
##' @S3method focus<- default
"focus<-.default" <- function(x, value) {
  x$set_focus(as.logical(value))
  x
}


##' Set a tooltip for the widget
##'
##' @param x object
##' @param value character tooltip value
##' @export
##' @rdname tooltip
"tooltip<-" <- function(x, value) UseMethod("tooltip<-")

##' Basic S3 method for tooltip<-
##'
##' @export
##' @rdname tooltip
##' @method tooltip<- default
##' @S3method tooltip<- default
"tooltip<-.default" <- function(x, value) {
  x$set_tooltip(paste(value, collapse="\n"))
  x
}

##' Toggle editability of the object, if supported
##'
##' @param x object. Calls objects \code{set_editable} method
##' @param ... passed to \code{set_editable} method
##' @param value logical
##' @export 
##' @rdname editable_assign
"editable<-" <- function(x, ..., value) UseMethod("editable<-")

##' assignment method for editable
##' 
##' @method editable<- GComponent
##' @S3method editable<- GComponent
##' @rdname editable_assign
"editable<-.GComponent" <- function(x,  ..., value) {
  x$set_editable(value, ...)
  x
}

## S3 generics that see use

##' Method for [
##' @param x object
##' @param i row index
##' @param j column index
##' @param ... passed to \code{get_items}
##' @param drop passed to \code{get_items}
##' @method [ GComponent
##' @S3method [ GComponent
##' @rdname bracket
"[.GComponent" <- function(x, i, j, ..., drop=TRUE) {
  x$get_items(i, j, ...)
}

##' assignment method for "["
##' @param x objecct
##' @param i row
##' @param j column
##' @param ... passed to \code{set_items}
##' @param value passed to \code{set_items}
##' @method [<- GComponent
##' @S3method [<- GComponent
##' @rdname bracket_assign
"[<-.GComponent" <- function(x, i, j, ..., value) {
  if(missing(i) && missing(j))
    x$set_items(value, ...)
  else if(missing(j))
    x$set_items(value, i, ,...)
  else if(missing(i))
    x$set_items(value, , j, ...)
  else
    x$set_items(value, i, j, ...)
  x
}

##' method for names
##' @param x object 
##' @method names GComponent
##' @S3method names GComponent
names.GComponent <- function(x) x$get_names()

##' assignment method for names
##' @param x object
##' @param value new names
##' @method names<- GComponent
##' @S3method names<- GComponent
##' @aliases namesGComponent
##' @rdname names_assign
"names<-.GComponent" <- function(x, value) {
  x$set_names(value)
  x
}



##' length method for GComponent's
##'
##' This definition has some sideeffects. Namely, if one uses \code{sapply} and
##' returns such objects, then the call to \code{simplify2array} will cause an error.
##' Use \code{lapply}, or \code{simplify=FALSE}.
##' @param x object
##' @param ... passed along to \code{len} method
##' @return length of object, loosely interpreted
##' @method length GComponent
##' @S3method length GComponent
length.GComponent <- function(x, ...) x$len(...)


##' dim method for GComponent's
##'
##' Return size of component, when rectangular size makes sense
##' @param x object
##' @param ... passed along to \code{len} method
##' @return dim of object, loosely interpreted
##' @method dim GComponent
##' @S3method dim GComponent
dim.GComponent <- function(x, ...) x$get_dim(...)

##' method for update
##' @param object object
##' @param ... passed along
##' @method update GComponent
##' @S3method update GComponent
update.GComponent <- function(object, ...) object$update(...)


##' Insert text into \code{gtext}
##'
##' Used to insert text into \code{gtext} insances
##' @param x object
##' @param value text value
##' @param where where to insert \code{c("end", "beginning", "at.cursor")}
##' @param font.attr ignored. Font attribute for new text
##' @param do.newline Do we add a new line?
##' @param ... ignored
##' @return no useful return value
##' @export
insert <- function (x, value, where = "end", 
                    font.attr = NULL, do.newline = TRUE, ...)  UseMethod("insert")

##' method for insert
##' @param x object
##' @param value value to insert
##' @param where to insert
##' @param font.attr ignored
##' @param do.newline logical
##' @param ... ignored 
##' @method insert GComponent
##' @S3method insert GComponent
insert.GComponent <- function (x, value, where = "end", 
                              font.attr = NULL, do.newline = TRUE, ...)  {
  x$insert(value, where=match.arg(where,c("end", "beginning", "at.cursor")), do.newline=do.newline)
}

##################################################
## container methods

##' Add child component to parent container
##'
##' Usually called internally by specifying \code{container=parent} to the constructor
##' @param x object
##' @param child child object
##' @param ... Not implemented. For values like \code{expand}, \code{fill} and \code{anchor}
##' @return nothing
##' @export
add <- function(x, child, ...) UseMethod("add")

##' method for add
##' @param x object
##' @param child child object
##' @param ... Not implemented. For values like \code{expand}, \code{fill} and \code{anchor}
##' S3 method for add and GComponent
##' @method add GComponent
##' @S3method add GComponent
add.GComponent <- function(x, child, ...) x$add(child, ...)

##' Delete child object
##'
##' @param x object
##' @param child child objecct
##' @param ... ignored
##' @export
delete <- function(x, child, ...) UseMethod("delete")

##' delete method
##' @param x object
##' @param child object
##' @param ... ignored
##' @method delete GComponent
##' @S3method delete GComponent
delete.GComponent <- function(x, child, ...) x$delete(child, ...)


##' Dispose (delete) object
##'
##' @param x object
##' @param ... ignored
##' @export
dispose <- function(x,...) UseMethod("dispose")

##' Method for dispose
##' @param x object
##' @param ... ignored
##' @method dispose GComponent
##' @S3method dispose GComponent
dispose.GComponent <- function(x,...) x$dispose(...)

##' Add a spring to push additionnal child objects to far end of packing
##'
##' For box Containers only
##' @param x a box container (e.g., \code{ggroup})
##' @return NULL
##' @export
addSpring <- function(x) UseMethod("addSpring")

##' Method for addSpring
##' 
##' @inheritParams addSpring
##' @method addSpring default
##' @S3method addSpring default
addSpring.default <- function(x) {}
  
##' Method for addSpring
##' 
##' @inheritParams addSpring
##' @method addSpring GGroup
##' @S3method addSpring GGroup
addSpring.GGroup <- function(x) {
  ## just expand an empty label
  glabel(" ", container=x, expand=TRUE)
}

##################################################
## Handler calls

##' Assign handler to default event
##'
##' The default event is specified in the definition of the
##' object. Using this function allows one to have a "generic" name
##' for adding a handler.
##' @param x the object
##' @param ... Used to pass through \code{handler}, and \code{action}
##' values. The handler is a function whose first argument is a list
##' that contains components \code{obj} to return the object and
##' \code{action}, holding the action value, and possibly others.
##' @export
addHandlerChanged <- function(x, ...) x$add_handler_changed(...)

##' Assign handler to a change of selection event
##'
##' Some widgets have selection, this allows a callback to be changed when that occurs
##' @param x the object
##' @param ... Used to pass through \code{handler}, and \code{action}
##' values. The handler is a function whose first argument is a list
##' that contains components \code{obj} to return the object and
##' \code{action}, holding the action value, and possibly others.
##' @export
addHandlerSelectionChanged <- function(x, ...) x$add_handler_selection_changed(...)

##' Assign handler to click event
##'
##' @param x the object
##' @param ... Used to pass through \code{handler}, and \code{action}
##' values. The handler is a function whose first argument is a list
##' that contains components \code{obj} to return the object and
##' \code{action}, holding the action value, and possibly others.
##' @return a callback id, used with \code{removeHandler}
##' @export
addHandlerClicked <- function(x, ...) x$add_handler_clicked(...)

##' Assign handler to double click event
##'
##' @param x the object
##' @param ... Used to pass through \code{handler}, and \code{action}
##' values. The handler is a function whose first argument is a list
##' that contains components \code{obj} to return the object and
##' \code{action}, holding the action value, and possibly others.
##' @return a callback id, used with \code{removeHandler}
##' @export
addHandlerDoubleClick <- function(x, ...) x$add_handler_double_click(...)

##' Assign handler to click event for column (gdf, gtable)
##'
##' @param x the object
##' @param ... Used to pass through \code{handler}, and \code{action}
##' values. The handler is a function whose first argument is a list
##' that contains components \code{obj} to return the object and
##' \code{action}, holding the action value, and possibly others.
##' @return a callback id, used with \code{removeHandler}
##' @export
addHandlerColumnClicked <- function(x, ...) x$add_handler_column_clicked(...)

##' Assign handler to double click event for column (gdf, gtable)
##'
##' @param x the object
##' @param ... Used to pass through \code{handler}, and \code{action}
##' values. The handler is a function whose first argument is a list
##' that contains components \code{obj} to return the object and
##' \code{action}, holding the action value, and possibly others.
##' @return a callback id, used with \code{removeHandler}
##' @export
addHandlerColumnDoubleClick <- function(x, ...) x$add_handler_column_double_click(...)


##' Assign handler to mouse motion event
##'
##' This handler gets passed back when the mouse moves. This can be really slow!
##' @param x the object
##' @param ... Used to pass through \code{handler}, and \code{action}
##' values. The handler is a function whose first argument is a list
##' that contains components \code{obj} to return the object and
##' \code{action}, holding the action value, and possibly others.
##' @return a callback id, used with \code{removeHandler}
##' @export
addHandlerMouseMotion <- function(x, ...) x$add_handler_mouse_motion(...)


##' Assign handler to keystroke event
##'
##' This handler gets passed back the key that was pressed in the
##' \code{key} component.
##' @param x the object
##' @param ... Used to pass through \code{handler}, and \code{action}
##' values. The handler is a function whose first argument is a list
##' that contains components \code{obj} to return the object and
##' \code{action}, holding the action value, and possibly others.
##' @return a callback id, used with \code{removeHandler}
##' @export
addHandlerKeystroke <- function(x, ...) x$add_handler_keystroke(...)

##' Assign handler to focus in event
##'
##' @param x the object
##' @param ... Used to pass through \code{handler}, and \code{action}
##' values. The handler is a function whose first argument is a list
##' that contains components \code{obj} to return the object and
##' \code{action}, holding the action value, and possibly others.
##' @return a callback id, used with \code{removeHandler}
##' @export
addHandlerFocus <- function(x, ...) x$add_handler_focus(...)

##' Assign handler to blur (lose focus) event
##'
##' @param x the object
##' @param ... Used to pass through \code{handler}, and \code{action}
##' values. The handler is a function whose first argument is a list
##' that contains components \code{obj} to return the object and
##' \code{action}, holding the action value, and possibly others.
##' @return a callback id, used with \code{removeHandler}
##' @export
addHandlerBlur <- function(x, ...) x$add_handler_blur(...)

##' Assign handler to destroy event (such as page being unloaded)
##'
##' @param x the object
##' @param ... Used to pass through \code{handler}, and \code{action}
##' values. The handler is a function whose first argument is a list
##' that contains components \code{obj} to return the object and
##' \code{action}, holding the action value, and possibly others.
##' @return a callback id, used with \code{removeHandler}
##' @export
addHandlerDestroy <- function(x, ...) x$add_handler_destroy(...)

##' Assign handler to be called repeatedly after some interval
##'
##' @param x the object
##' @param interval interval in ms
##' @param handler handler to call
##' @param action optinal value to pass to handler
##' @return a callback id, used with \code{removeHandler}
##' @export
addHandlerIdle <- function(x, interval=1000, handler, action=NULL)
  x$add_handler_idle(interval, handler, action)



##' Remove handler
##'
##' @param x the object handler is called on.
##' @param cbid the callback id returned by an addHandlerXXX call
##' @return NULL
##' @note see the reference method \code{remove_handlers} to remove
##' all handlers on a widget.
##' @export
removeHandler <- function(x, cbid) x$remove_handler(cbid)


##' temporariliy block handler by id
##'
##' @param x the object handler is called on.
##' @param cbid the callback id returned by an addHandlerXXX call
##' @return NULL
##' @note see the reference method \code{block_handlers} to block all
##' handlers for the widget.
##' @export
blockHandler <- function(x, cbid) x$block_handler(cbid)



##' unblock handler by id
##'
##' @param x the object handler is called on.
##' @param cbid the callback id returned by an addHandlerXXX call
##' @return NULL
##' @note see the reference method \code{unblock_handlers} to unblock
##' all the handlers for a widget.
##' @export
unblockHandler <- function(x, cbid) x$unblock_handler(cbid)

