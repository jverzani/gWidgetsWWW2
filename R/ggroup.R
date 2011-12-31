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


##'  Group, or box, container
##'
##' A box container. Can pack in left to right or top to bottom.
##' @param horizontal left or right (default), or top to bottom (\code{horizontal=FALSE})
##' @param spacing body spacing
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
                   spacing=5,
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


##' base class for ggroup
##'
##' For \code{GGroup}  and its subclasses, boxes are different. For
##' vertical boxes they need to have a height set. We use a default of
##' 200.
##' 
##' The standard expand, fill, anchor arugments are not implemented as
##' they are in RGtk2, say.
##'
##' The expand maps to flex which is a weight for stretching the
##' object in the packing direction. That is for a hbox, it will
##' strecth in horizontal direction
##'
##' The fill corresponds to the align argument for the container --
##' not the component. There is no way to fill just one. The fill
##' value of "stretch" will stretch the component in the orthogonal
##' direction to filling. Passing the value "stretchmax" should stretch
##' to the largest child size, but not the max.
##'
##' So expand=TRUE, fill=TRUE will will stretch in both
##' directions.
##'
##' But wait, the fill value -- since it appears to the container, but
##' is specified by the children can be specified more
##' than once. The last one wins.
##'
##' When expand and fill are not used, anchoring should be
##' possible. The CSS class needs to be set up properly though.
##' @rdname gWidgetsWWW2-package
GGroup <- setRefClass("GGroup",
                       contains="GContainer",
                       fields=list(
                         "horizontal"="logical"
                         ),
                       methods=list(
                         init = function(
                           horizontal=TRUE,
                           spacing=5,
                           use.scrollwindow = FALSE,
                           container,
                           ...,
                           width =NULL,
                           height=NULL,
                           ext.args = NULL
                           ) {

                           constructor <<- "Ext.Panel"
                           spacing <<- spacing
                           horizontal <<- horizontal

                           ## give a default height if horizontal=FALSE
                           ## height <- getWithDefault(height, default=if(!horizontal) 200 else NULL)

                           
                           ## spacing goes around inner margins of the box
                           ## padding goes between objects. Here we use the same value
                           ## for each
                           
                           arg_list <- list(border = TRUE,
                                            hideBorders = FALSE,
                                            anchor="100%",
                                            width=width,
                                            height=height,
                                            defaults=list(margins=sprintf("%s %s %s %s", spacing, spacing, spacing, spacing))
                                            )
                           if(use.scrollwindow) {
                             arg_list[['autoScroll']] <- TRUE
                             if(is.null(height))
                               arg_list[['autoHeight']] <- TRUE
                           }

                           ## From HBox.js:
                           ## * - **stretch** : child items are stretched vertically to fill the height of the container
                           ## * - **stretchmax** : child items are stretched vertically to the height of the largest item.      
                           if(horizontal)
                             arg_list[['layout']] <- list(type="hbox", padding=spacing, align="top")
                           else
                             arg_list[['layout']] <- list(type="vbox", padding=spacing, align="left")
                           

                           add_args(arg_list)
                           
                           if(!is.null(ext.args))
                             args$extend(ext.args)
                           
                           container$add_dots(.self, ...)                           

                           write_constructor()
                           add_js_queue(sprintf("var %s=%s.getComponent('%s_status_bar')",
                                                status_id(), get_id(), get_id()))
                           add_js_queue(sprintf("var %s=%s.getComponent('%s_toolbar')",
                                                toolbar_id(), get_id(), get_id()))
                                                     
                           container$add(.self, ...)
                           
                         },
                         get_items= function(i, j, ...) {
                           children$core()[i, ...]
                         }

                           
                         )
                       )
