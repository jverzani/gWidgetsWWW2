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

## When all is said and done this should end up in gWidgets2, not
## gWidgetsWWW2. But that only happens when gWidgetsWWW2 is rewritten
## to use the standard reference class interface and is called
## gWidgets2WWW.


##' @include array.R
##' @include utils.R
NULL

##' Return x unless NULL, NA, length 0, ..., in which case we give default
##'
##' @param x value to return or its default
##' @param default default value
##' @return x or default
getWithDefault <- function(x, default) {
  if(is_empty(x))
    default
  else
    x
}


##' is value missing, null, 0-length or NA length 1
##'
##' @param x object to test
##' @return logical
is_empty <- function(x) {
  missing(x) ||
  is.null(x) ||
  (length(x) == 0) ||
  (is.atomic(x) && length(x) == 1 && is.na(x))
}

##' Functions to message something needs doing. Easy to search for
##'
##' @param msg optional message to emit
XXX <- function(msg) {
  if(!missing(msg))
    message(msg)
}



## some special class unions so we can have easier to deal with default
setClassUnion("IntegerOrNULL", c("integer", "NULL"))
setClassUnion("CharacterOrNULL", c("character", "NULL"))
setClassUnion("LogicalOrNULL", c("logical", "NULL"))
setClassUnion("LogicalCharacterOrNULL", c("logical", "character", "NULL"))



## simple message function
define_me <- function(...) {
  curcall <- as.character(sys.call()[[1]])[3]
  message(sprintf("Method %s not defined for class %s\n",
                  curcall,
                  class(get("self"))          # issue with warning
                  ))
}

## Observer class
Observer <- R6Class("Observer",
  public=list(
    o = NULL,   
    obj = NULL,
    initialize = function(o=NULL, obj=NULL) {
      self$o <- o
      self$obj <- obj
    },
    update=function(...) {
      "Call self."
      self$o(self$obj, ...)
    }
  )
)

## Handler class
Handler <- R6Class("Handler",
  inherit = Observer,
  public = list(
    action = NULL,
    initialize=function(o=NULL, obj=NULL, action=NULL) {
      self$action <- action
      super$initialize(o=o, obj=obj)
    },
    update=function(x, ...) {
      "Call self."
      h <- list(obj=self$obj, action=self$action)
      if(!missing(x)) {
        h <- merge.list(h, x, overwrite=FALSE)
      }
      self$o(h, ...)
    }
  )
)

## create an observer using the gWidgets interface of handler, action
observer <- function(receiver, handler, action=NULL) 
  Handler$new(handler, receiver, action)


## Observable class sets up objects that can be observed. Inherited by template
Observable <- R6Class("Observable",
  public=list(
    ..observers = list(),
    ..blocked_observers = list(),
    ..blocked = integer(),
    add_observer=function(o, signal="DEFAULT") {
      "Add an observer. Return id for block/remove/..."
      if(!is(o, "Observer"))
        stop("Not an observer")
      l <- self$..observers
      if(is.null(l[[signal]]))
        l[[signal]] <- list(o)
      else
        l[[signal]] <- c(l[[signal]], o)
      self$..observers <- l
      list(signal=signal, o=o)
    },
    remove_observer=function(id) {
      "Remove observer"
      if(!is(id$o, "Observer"))
        stop("Call with an observer id")
      
      signal <- id$signal
      ind <- lapply(self$..observers[[signal]], function(i) identical(i, id$o))
      if(any(ind <- unlist(ind)) )
        self$..observers[[signal]][[which(ind)]] <- NULL
      
    },
    ## these block all observers
    block_observers=function() {
      "Block all observers"
      if(length(self$..blocked) == 0) {
        self$..blocked <- 1L
      } else {
        self$..blocked <- self$..blocked + 1L
      }
    },
    unblock_observers=function() {
      "Remove block of all observers. Keeps count, so may need to call again"
      if(length(self$..blocked) == 0) {
        self$..blocked <- 0L
      } else {
        self$..blocked <- max(self$..blocked - 1L, 0L)
      }
      invisible(self$..blocked)
    },
    ## These block/unblock one at a time
    block_observer=function(id) {
      "Block observers. If o missing, block all"
      if(missing(id) || is.null(id)) {
        self$block_observers()
      } else {
        if(is.null(self$..blocked_observers[[id$signal]]))
          self$..blocked_observers[[id$signal]] <- list(id$o)
        else
          self$..blocked_observers[[id$signal]] <-
            c(self$..blocked_observers[[id$signal]], id$o)
      }
    },
    unblock_observer=function(id) {
      "Unblock observer. If id missing, unblock global block"
      if(missing(id) || is.null(id)) {
        self$unblock_observers()
      } else {
        signal <- id$signal
        ind <- sapply(self$..blocked_observers[[signal]], function(i) identical(i, id$o))
        if(any(ind <- unlist(ind))) 
          self$..blocked_observers[[signal]][[which(ind)]] <- NULL
      }
    },
    notify_observers=function(..., signal="DEFAULT") {
      "Call each non-blocked observer"
      if(length(self$..blocked) && self$..blocked > 0)
        return()
      QT <- lapply(self$..observers[[signal]], function(o) {
        ind <- lapply(self$..blocked_observers[[signal]], function(i) identical(i, o))
        if(!any(unlist(ind))) 
          o$update(...)
        }
      )
    }
  )
)

## Basic interface for a widget. These are methods referenced by the S3 methods
##
## This interface is inherited by the base GComponent classes in the
## toolkit implementations. The methods defined here are referenced
## by the S3 methods. For exampe, \code{svalue} dispatches to
## \code{get_value}.
##
## We combine both widget and container methods here. It isn't
## perfect, but they do share quite a bit. Perhaps, we could make the
## container class subclass the basic interface.
BasicToolkitInterface <- R6Class("BasicToolkitInterface",
  inherit = Observable,
  public=list(
    toolkit=NULL,
    widget=NULL,
    block=NULL,
    parent=NULL, # NULL for gwindow, else parent container
    default_expand=NULL,
    default_fill=NULL,
    ## (drop=NULL, ...), (value, drop=TRUE, ...)
    get_value=define_me, # svalue
    set_value=define_me, # svalue<-
    ## (...), (value, ...)
    get_index=define_me, # svalue; index=TRUE
    set_index=define_me,   # svalue <-; index=TRUE
    ## (i, j, ..., drop=NULL)
    get_items=define_me,   # [
    ## (value, i, j, ...)
    set_items=define_me,   # [<-
    ## () and (value)
    get_enabled=define_me, # enabled
    set_enabled=define_me, # enabled<-
    get_visible=define_me, # visible
    set_visible=define_me, # visible<-
    get_editable=define_me, # editable
    set_editable=define_me, # editable<-
    get_focus=define_me,    # foucs
    set_focus=define_me,    # focus<-
    get_font=define_me,    # font
    set_font=define_me,    # font<-
    get_length=define_me,  # length
    set_length=define_me,  # length<-
    ##
    get_dim=define_me,     # dim
    get_names=define_me,   # names
    set_names=define_me,   # names<-
    get_dimnames=define_me, # dimnames
    set_dimnames=define_me, # dimnames <-
    get_attr=define_me,    # tag
    set_attr=define_me,    # tag<-
    update_widget=define_me, # update
    is_extant=function() TRUE,   # isExtant
    undo=define_me,          # undo
    redo=define_me,          # redo
    add_child=define_me,     # add child to container (if present)
    set_parent=function(parent) parent <- parent,

    ## (signal, handler, action=NULL, decorator, emitter)
    add_handler=define_me,
    ## (handler, action, ...)
    add_handler_changed=define_me,
    add_handler_clicked=define_me,
    add_handler_double_clicked=define_me,
    add_handler_right_clicked=define_me,
    add_handler_column_clicked=define_me,
    add_handler_column_double_clicked=define_me,
    add_handler_column_right_clicked=define_me,
    add_handler_select=define_me,
    add_handler_focus=define_me,
    add_handler_blur=define_me,
    add_handler_destroy=define_me,
    add_handler_unrealize=define_me,
    add_handler_expose=define_me,
    add_handler_keystroke=define_me,
    add_handler_mouse_motion=define_me,
    add_popup_menu=define_me,
    add_3rd_mouse_popup_menu=define_me,
    add_drop_source=define_me,
    add_drop_target=define_me,
    add_drag_Motion=define_me,
    emit_signal=define_me, # gSignalEmit, ... to invoke. Signal missing do change one
    ## show method
    show=function() {
      cat(sprintf("An object of class %s", class(self)[1]), "\n")
    }
))
