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

##' A paned group container
##'
##' This container has two children and a vertical/horizontal sash to
##' allocate space between them. Simply use as a parent container
##' twice. Unlike other gWidgets implementations, \code{svalue<-} is not implemented.
##' If in horizontal mode, the second widget added will be to the left.
##' @param horizontal logical. Side by side (or top/bottom) layout
##' @inheritParams gwidget
##' @param collapsible logical. If TRUE, one child will have a button so that it can be collapsed
##' @param default.size Integer pixel size of right or bottom
##' container when drawn. Defaults to half of height or width, or 200
##' if those are NULL. There is no \code{svalue<-} method to adjust the size
##' @return an ExtContainer object
##' @export
##' @examples
##' w <- gwindow()
##' pg <- gpanedgroup(cont=w, horizontal=TRUE)
##' gbutton("left", cont=pg)
##' gbutton("right", cont=pg)
gpanedgroup <- function(horizontal=TRUE, container=NULL, ...,
                        width=NULL, height=400, ext.args=NULL,
                        collapsible=FALSE, default.size=NULL
                        ) {
  pg <- GPanedGroup$new(container$toplevel)
  pg$init(horizontal, container, ...,
          width=width, height=height, ext.args=ext.args,
          collapsible=collapsible, default.size=default.size
          )
  pg
}

GPanedGroup <- setRefClass("GPanedGroup",
                           contains="GContainer",
                           fields=list(
                             child_ct = "numeric",
                             horizontal="logical"
                             ),
                           method=list(
                             init=function(horizontal, container, ...,
                               width=NULL, height=NULL, ext.args=NULL,
                               collapsible=FALSE, default.size=NULL
                               ) {
                               child_ct <<- 0
                               horizontal <<- horizontal
                               
                               
                               constructor <<- "Ext.Panel"
                               width_or_height <- ifelse(horizontal, "width", "height")
                               and_the_opposite <- ifelse(!horizontal, "width", "height")
                               west_or_south <- ifelse(horizontal, "west", "south")


                               default.size <- getWithDefault(default.size,
                                                              ifelse(is.null(get(width_or_height)), 200,
                                                                 get(width_or_height)/2))
                               collapsible <- getWithDefault(collapsible, FALSE)

                               ## This needs to be tidied up XXX
                               items <- paste("[{",
                                              sprintf("region:'%s',", west_or_south),
                                              sprintf("%s:%s,", width_or_height, default.size),
                                              if(!is.null(get(and_the_opposite)))
                                                sprintf("%s:%s,", and_the_opposite, get(and_the_opposite)),
                                              sprintf("id:'%s_%s',", get_id(), west_or_south),
                                              sprintf("collapsible: %s,", coerceToJSString(collapsible)),
                                              "split: true,",
                                              "layout: 'fit'",
                                              "},{",
                                              sprintf("id: '%s_center',", get_id()),
                                              "region: 'center',",    ## center region is required, no width/height specified
                                              "xtype: 'container',",
                                              "layout: 'fit'",
                                              "}]",
                                              sep="")
                               
                               arg_list <- list(
                                                layout="border",
                                                width=width,
                                                height=height,
                                                items=String(items)
                                                )

                               args$extend(arg_list, ext.args)
                               write_constructor()

                               container$add(.self, ...)
                             },
                             add = function(child, where=NULL, ...) {
                               ## where can be compass point. First child is in "center"
                               if(is.null(where)) {
                                 if(child_ct == 0)
                                   where <- "center"
                                 else if(child_ct == 1)
                                   where <- ifelse(horizontal, "west", "south")
                                 else
                                   return()
                               }
                               
                               cmd <- paste(sprintf("var tmp = Ext.getCmp('%s_%s');", get_id(), where),
                                            sprintf("tmp.add('%s');", child$id), # not get_id()!
                                            'tmp.doLayout();',
                                            sep="")
                               add_js_queue(cmd)

                               child_ct <<- child_ct + 1
                             },

                             set_value = function(value, ...) {
                               "Set sash position, if possible"
                               cat("XXX not implemented")
                             }
                             
                             ))
