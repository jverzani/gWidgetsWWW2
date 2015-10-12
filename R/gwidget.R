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

##' Stub for roxygen2 documentation of common arguments for gWidgets constructors
##' 
##' @param handler optional means (to \code{\link{addHandlerChanged}})
##' to specify a handler for the default signal. A handler is a
##' function with signature \code{(h,...)} where \code{h} is a list
##' with components \code{obj} referring to the object emitting the
##' signal, \code{action} containing values passed to the
##' \code{action} argument, and possible other values.
##' @param action Passed to handler to parameterize a call
##' @param container A parent container. In \pkg{gWidgetsWWW2} a
##' parent container is not optional (though it can be substituted
##' with the \code{parent} argument in some circumstances). The parent
##' specifies the widget hierarchy and the \code{...} argument is used
##' to pass along arguments to layout the child component in the
##' parent container. Typically, these are passed to the \code{add}
##' method of the parent container.
##' @param ... Used to pass along argument to the parent container's
##' \code{add} method and possible other arguments to the underlying
##' reference class constructors.
##' @param width width in pixels of component. Sizing in
##' \pkg{gWidgetsWWW2} is sometimes necessary as the arguments
##' \code{expand} and \code{fill} are not well implemented.
##' @param height height in pixels of the component.
##' @param ext.args The constructors of \pkg{gWidgetsWWW2} ultimately
##' call an Ext constructor. The options passed to the Ext constructor
##' may be added to or overridden by use of this argument. Values are
##' passed in as named list components and with values converted into JavaScript
##' objects by \code{asJSObject}.
##' @param expand Logical or numeric. Does the child component expand
##' to fill the allotted space? If so a \code{flex} value can be
##' specified (as a positive integer) that gives weights to all
##' children of the parent container when they expand.
##' @param fill Logical or character. When \code{expand=TRUE}, the
##' space allotted to the component grows, but the widget itself may
##' not. The \code{fill} argument is a logical value or character
##' string from "both", "x", or "y", where "both" is the same as
##' \code{TRUE}. These refer to expansion directions. The widget's may
##' have defaults for \code{expand} and \code{fill}. Typically, when
##' packing into box containers, child components expand in the
##' orthogonal direction (horizontal boxes have vertical, \code{y},
##' fill values).
##' @param anchor specifies where a widget is anchored (using a value
##' in {-1, 0,1} cross {-1, 0, 1}), in the case the widget
##' has more space allocated to it then requested.
gwidget <- function(handler, action=NULL, container=NULL, ...,
                    width=NULL, height=NULL, ext.args=NULL,
                    expand=NULL, fill=NULL, anchor=NULL
                    ) {
  ## A documentation stub only
}


##' \code{GWidget} is the base class for widgets
##'
##' \code{GWidget} is the base class for widget objects. See
##' \code{GContainer} for the base class for container objects. Both
##' derive from the \code{GComponent} class.
##' @rdname gWidgetsWWW2-package
GWidget <- R6Class("GWidget",
  inherit = GComponent,
  public = list(
    initialize=function(..., coerce.with) {
      if(!missing(coerce.with)) {
        if(is.character(coerce.with))
          self$coerce_with <- get(coerce.with, inherits=TRUE)
        else
          self$coerce_with <- coerce.with
      }

      super$initialize(...)
    }
  )
)
                     
## Needs subclasses:
## GWidgetWithItems (gradio, ...)
## GWidgetWithProxy (ghtml, gtable, gdf, ...)
## GWidgetText      (gtext)
