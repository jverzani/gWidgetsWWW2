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

##' @include gcontainer.R
NULL

## TODO: anchoring of widgets


##' Group, or box, container
##'
##' @param horizontal left or right (default), or top to bottom (\code{horizontal=FALSE})
##' @param spacing Margin around each child component in pixels. Can
##' be a single number, in which case it is equal pixel space around
##' each child. But for gWidgetsWWW2 one can specify a vector with
##' recycling like function(top, right=top, bottom=top, left=right). A
##' typical pattern is c(5,5,0,0), as otherwise there are 10 = 5 + 5 pixels
##' between adjoing children. To get padding just around interior of
##' box, pass in a value through ext.args, as in
##' \code{ext.args=list(bodyPadding=10)}.
##' @param use.scrollwindow logical. If given, scrollbars will appear
##' @inheritParams gwidget
##' @return a \code{GGroup} reference class  object
##' @export
##' @examples
##' w <- gwindow()
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
##' g <- ggroup(cont=w, horizontal=FALSE) ## top to bottom
##' for(i in 1:10) gbutton(i, cont=g)
##' ## add/delete
##' ctr <- 1 ### for label
##' addRow <- function(g) {
##' g1 <- ggroup(cont=g)
##' gbutton("x", cont=g1, handler=function(h,...) {delete(g, g1)})
##' glabel(paste("Click x to delete", ctr), cont=g1)
##' ctr <<- ctr + 1
##' }
##' g1 <- gframe("add/delete", cont=g, horizontal=FALSE)
##' addRow(g1)
##' f <- gframe("Adding and deleting", cont=g, horizontal=FALSE)
##' gbutton("+", cont=f, handler=function(h,...) addRow(f))
ggroup <- function(
                   horizontal=TRUE,
                   spacing=2,
                   use.scrollwindow = FALSE,
                   container,
                   ...,
                   width=NULL,
                   height=NULL,
                   ext.args = NULL
                   ){
  g <- GGroup$new(container, ...)
  g$init(horizontal, spacing, use.scrollwindow, container, ..., width=width, height=height, ext.args=ext.args)
  g
}

##' Shortcut for vertical box containers. 
##'
##' @param ... passed to \code{ggroup}
##' @export
##' @rdname ggroup
gvbox <- function(...) {
  f <- function(..., horizontal) ggroup(..., horizontal=FALSE)
  f(...)
}

##' \code{GGroup} is the base class for ggroup and all box containers/
##'
##' Box containers pack in child objects in a left to right manner
##' (horizontal) or top to bottom (horizonta=FALSE). In the gWidgets
##' API the space allocated to a child dependsd on the arguments
##' expand, fill and anchor. These have a different interpretation in
##' gWidgetsWWW2, as there are no analogs in Ext's Box layouts.
##'
##' Let's discuss vertical box containers, turn this on the side to
##' think about horizontal ones. A vertical box container places its
##' children from top to bottom. When packing each child has its
##' requested height. If the sum of these is less than the height of
##' the box (and no flex values is given) then there is excess space
##' in the bottom of the box. If the sum is greater, the argument
##' \code{use.scrollwindow} should make it so that parent box
##' containers has scrollbars allowing the user to scroll to see the
##' bottom widgets. This has been an issue getting to work though.
##'
##' Now, when there is more availabe verticle space than requested
##' space, we can also have the widgets request more availabel
##' space. The expand argument can be a logical indicating if the
##' available space to a widget should expand to fill the possible
##' space. In Ext, this maps to a value of flex, which is a weight so
##' child items with bigger flex values get a weighted amount of
##' space. As such, you can specify a positive number to the expand
##' argument. A logical is simply converted to 0 or 1;
##'
##' As for horizontal space, the FILL argument should be used to
##' indicate if a child item should expand in the horizontal space
##' allotted. However, Ext doesn't allow per item sizing of children,
##' rather the 'align' property is a property of the parent container
##' (the box). We set it to 'stretch'. This gives similar behaviour as
##' RGtk2, where child components always stretch to fill the direction
##' orthogonal to the packing direction. (This can lead to undesirable
##' layouts. To override, pass in something like
##' \code{ext.args=list(layout=list(type="hbox",align="top"))}, say.)
##' 
##'
##' This can lead to large box containers, when a box container is
##' also a child of a parent box container. The size of the child box
##' can be controlled through the width and height arguments. These
##' map to \code{maxWidth} and \code{maxHeight}, not the usual
##' \code{width} and \code{height} which are really requests, but
##' additional space can be allocated.
##'
##' The anchor argument is meant to locate the actual widget within
##' the space allocated to the widget. In the gWidgets spec if
##' expand=TRUE, fill=FALSE, then the widget may have additional space
##' to fill than it needs. The anchor specifies in x and y coordinates
##' where it should go values are in {-1,0,1} x {-1, 0, 1}. 
##' @rdname gggroup
GGroup <- setRefClass("GGroup",
                       contains="GContainer",
                       fields=list(
                         "horizontal"="logical"
                         ),
                       methods=list(
                         init = function(
                           horizontal=TRUE,
                           spacing=2, 
                           use.scrollwindow = FALSE,
                           container,
                           ...,
                           width =NULL,
                           height=NULL,
                           ext.args = NULL
                           ) {

                           constructor <<- "Ext.panel.Panel"
                           horizontal <<- horizontal
                           ## get spacing right
                           margins <- spacing
                           if(length(spacing) == 1)
                             margins <- rep(spacing, 4)
                           else if(length(spacing) == 2)
                             margins <- rep(spacing, 2)
                           else if(length(spacing) == 3)
                             margins <- c(spacing, spacing[2])
                           else
                             margins <- spacing[1:4]
                           spacing <<- margins

                           ## we put margins in defaults so that it
                           ## goes around each child we add, this
                           ## effectively puts space around each
                           ## child.
                           arg_list <- list(border = FALSE,
                                            hideBorders = FALSE,
                                            defaults=list(
                                              margins=sprintf("%s %s %s %s", margins[1], margins[2], margins[3], margins[4])
                                              )
                                            )

                           if(!is.null(use.scrollwindow) && use.scrollwindow) {
                             arg_list[['autoScroll']] <- TRUE
                             if(is.null(height))
                               arg_list[['autoHeight']] <- TRUE
                           }

                          

                           
                           ## From HBox.js:
                           ## * - **stretch** : child items are stretched vertically to fill the height of the container
                           ## * - **stretchmax** : child items are stretched vertically to the height of the largest item.      
                           if(horizontal)
                             arg_list[['layout']] <- list(type="hbox", align="stretch") # was 'top'
                           else
                             arg_list[['layout']] <- list(type="vbox", align="stretch") # was 'left'
                           

                           add_args(arg_list, ext.args)

                           container$add_dots(.self, ...)
                           write_constructor()
                           container$add(.self, ...)
                           set_size(list(width=width, height=height))
                         },
                         get_items= function(i, j, ...) {
                           children$core()[i, ...]
                         }
                         )
                       )
