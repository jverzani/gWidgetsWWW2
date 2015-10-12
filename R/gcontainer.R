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

##' @include gcomponent.R
NULL

##' Base reference class for container objects.
##'
##' Container objects are those that hold other components and may be
##' passed to constructors through their \code{container}
##' argument. All objects keep a reference to their parent container,
##' containers also keep references to their child components. See the
##' \code{children} array, which is used for this.
##'
##' The main method for containers is the \code{add} method, though
##' this is rarely called, as the underlying method is used in a
##' widget's constructor when a parent container is specified through
##' the standard \code{container} argument.
##'
##' When an object is added one may need to recompute the layout. The
##' \code{do_layout} method will inittiate this. To have this done
##' each time a child is added set the argument \code{auto_layout} to
##' \code{TRUE}.
GContainer <- R6Class("GContainer",
  inherit = GComponent,
  public = list(
    children = NULL,
    auto_layout = FALSE,
    spacing = 5,
    initialize = function(...) {
      self$children = gWidgetsWWW2:::Array$new()
      super$initialize(...)
    },
    child_bookkeeping = function(child) {
      "Update parent property of child and children property of parent container"
      if(is(child, "GComponent"))
        child$set_parent(self)
      self$children$push(child, child$get_id())
    },
    add = function(child, expand, anchor, fill, ...) {
      ## For compatibility with previous
      ## we remove expand, anchor, fill as these are done through add_dots.
      self$add_child(child,  ...)
    },
    add_child = function(child,  ...) {
      "Add child to parent, do internal book keeping"
      child_bookkeeping(child)
      self$call_Ext("add", String(child$get_id())) # add to GUI
      if(self$auto_layout)
        self$do_layout()
    },
    update = function(...) {
      "Update method for containers refreshes the layout"
      self$do_layout()
    },

    add_dots = function(child, expand=FALSE, fill=FALSE, anchor=NULL, horizontal = TRUE, ...) {
      "Function to process expand, fill, anchor arguments when adding a child"
      ## This gets called before constructor
      ## is written, as we modify the args of
      ## the child component. Typical call (as in setup) is
      ## add_dots, write_constructor, container$add
      
      
      ## ## spacing first
      ## if(is.numeric(spacing))
      ##   child$add_args(list(style=list(padding=sprintf("%spx",spacing))))
      ## else if(is.character(spacing) && nchar(spacing))
      ##   ## eg spacing="'5px,0px,0px,5px'"
      ##   child$add_args(list(style=list(padding=spacing)))
      
      ## expand, anchor fill
      ## expand -- turned into flex value below
      expand <- getWithDefault(expand, default=child$default_expand)
      fill <- getWithDefault(fill, default=child$default_fill)
      anchor <- getWithDefault(anchor, default=NULL)

      ## turn expand into number 0=FALSE
      ## this way flex can vary with default of TRUE=1
      expand <- as.numeric(expand)
      if(expand)
        child$add_args(list(flex=expand))

      ## fill goes
      self$set_child_fill(child, fill, horizontal)

      
      if((is.null(fill) || (is.logical(fill) && !fill)) &&
         (is.null(expand) || !expand)) {
        if(!is.null(anchor))
          child$add_args(list(cls=mapAnchorToCSSClass(anchor)))
      }
      
      
    },
    set_child_align = function(child, alt_child, anchor) {
    },
    set_child_fill = function(child, fill, horizontal = TRUE) {
      "Fill can be NULL, TRUE, FALSE, '', 'both', 'x', 'y'..."
      ## XXX The align needs to be adjusted here.
      if(!is.null(fill)) {
        fill <- switch(as.character(fill),
                       "TRUE"="stretch",
                       "both"="stretch",
                       "x"=ifelse(has_slot("horizontal") &&  !horizontal, "stretch", "left"),
                       "y"=ifelse(has_slot("horizontal") &&  horizontal, "stretch", "top"),
                       fill)
        ## add to already constructed container:
        ## XXX not if FALSE: add_js_queue(sprintf("%s.layout.align='%s';", get_id(), fill))
      }
    },
    delete = function(child, ...) {
      "Remove child from container"
      self$children$remove_by_name(child$get_id())
      self$call_Ext("remove", String(child$get_id()))
    },
    do_layout = function() {
      "Call layout method of container to recompute"
      self$call_Ext("doLayout") # call update method if needed
    },
    ## toolbar/status methods. Should be just
    ## for Panel objects (such as gwindow, and
    ## ggroup, but we stuff in here
    ## nonetheless, as we don't have such a
    ## subclass)
    add_statusbar = function(status, ...) {
      cmd <- sprintf("%s.addDocked(%s);",
                     self$get_id(),
                     status$get_id())
      self$add_js_queue(cmd)
      self$do_layout()
    },
    add_toolbar = function(status, ...) {
      cmd <- sprintf("%s.addDocked(%s);",
                     self$get_id(),
                     status$get_id())
      self$add_js_queue(cmd)
      self$do_layout()
    },

    ## gWidgets methods
    set_enabled = function(value, ...) {
      "Recursively enable/disable child components"
      if(value)
        call_Ext("cascade", String("function(){ this.enable()}"))
      else
        call_Ext("cascade", String("function(){ this.disable()}"))
    },
    
    set_visible = function(value) {
      "Show container and its siblings"
      if(value)
        call_Ext("show")
      else
        call_Ext("hide")
    }
  )
)

   

