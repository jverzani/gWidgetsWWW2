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


##'  A table container
##'
##' A table container allows one to organize child widgets in a grid
##' using matrix notation to indicate where the child should be
##' placed. The basic assignment is: \code{tbl[i,j] <- gbutton("asdf",
##' cont=tbl)}. The \code{tbl} object is on the right side, so the
##' widget has a toplevel and on the left to specify how the layout
##' will occur. Although one can specify i and j as a range of values,
##' or even empty one must be careful that no "holes" would be left
##' over, as otherwise the layout will not work out correctly.
##'
##' The layout is only finalized after call the \code{visible<-}
##' method with a value of \code{TRUE}. One adds all the desired
##' children, then calls this method.
##' 
##' @param homogeneous equal sized columns/rows?
##' @param spacing between cell spacing
##' @inheritParams gwidget
##' @return an ExtContainer object
##' @export
##' @examples
##' w <- gwindow("grid layout example")
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
##' tbl <- glayout(cont=w)
##' tbl[1,1, anchor=c(1,0)] <- "Name"  ## a character maps to a label
##' tbl[1,2] <- gedit("", cont=tbl) ## tbl needed on right side too
##' tbl[2,1, anchor=c(1,0)] <- "Rank"
##' tbl[2,2] <- gedit("", cont=tbl)
##' visible(tbl) <- TRUE ## This line is needed!
glayout <- function(homogeneous = FALSE, spacing = 2, # 10 is too big here
                    container = NULL, ...,
                    width=NULL, height=NULL, ext.args=NULL
                    ) {

  l <- GLayout$new(container,...)
  l$init(homogeneous, spacing, container, ...,
         width=width, height=height, ext.args=ext.args)
  l
}

GLayout <- setRefClass("GLayout",
                       contains="GContainer",
                       fields=list(
                         widgets="Array",
                         container="ANY"
                         ),
                       methods=list(
                         init=function(homogeneous=FALSE,
                           spacing=5,
                           container,
                           ...,
                           width=NULL,
                           height=NULL,
                           ext.args=NULL) {

                           widgets <<- Array$new()
                           container <<- container
                           spacing <<- spacing
                           
                           constructor <<- "Ext.Panel"
                           arg_list <- list(layout="table",
                                            defaults=list(
                                              bodyStyle = sprintf("padding:%spx;", spacing)
                                              )
                                            )
                           add_args(arg_list)

                           container$add_dots(.self, ...)                           


                           ## we write constructor in set_visible
                         },
                         add=function(...) {
                           "Override add method, do nothing"
                         },
                         set_items=function(child, i, j, ...) {
                           "Add child at specified location"

                           ## can use character for labels
                           if(is.character(child)) 
                             child <- glabel(child, container=.self)
                           
                           
                           children$push(child, child$get_id())

                           if(missing(i)) i <- NA; if(missing(j)) j <- NA

                           l <- merge(list(widget=child, i=i, j=j),
                                      list(...),
                                      overwrite=FALSE)
                           widgets$push(l)
                         },
                         set_visible = function(value, ...) {
                           if(!value) return()
                           ## set row and colspans for each widget
                           widgets$each(function(i, key, value) {
                             f <- function(x, default) if(is.na(diff(range(x)))) default else 1 + diff(range(x))
                             rowspan <- f(value$i, no_rows())
                             colspan <- f(value$j, no_rows())

                             anchorCls <- mapAnchorToCSSClass(value$anchor)

                             ## add row and column span attributes to child widget etc:
                             l <- list(rowspan=rowspan,
                                       colspan = colspan,
                                       cellCls = anchorCls,
                                       style=list(paddding=sprintf("%spx", spacing))
                                       )
                             value$widget$ext_apply(l)
                           })


                           
                           ## now we add more arguments, then write constructor
                           arg_list <- list(layoutConfig=list(
                                              columns=no_columns()
                                              ),
                                            items=String(sprintf("[%s]",
                                              paste(sapply(widgets$core(), function(a) a$widget$get_id()), collapse=",")
                                            ))
                                            )
                           add_args(arg_list)
                           write_constructor()
                           container$add(.self, ...)
                         },
                         set_child_fill=function(child, fill, ...) {
                           ## XXX not defined
                         },
                         no_columns = function() {
                           "How many columns? Needed in config"
                           cols <- unlist(widgets$pluck("j"))
                           max(cols, na.rm=TRUE)
                         },
                         no_rows = function() {
                           "How many rows?"
                           rows <- unlist(widgets$pluck("i"))
                           max(rows, na.rm=TRUE)
                         },
                         .get_widget = function(i, j) {
                           "Return widget (or NULL) occupying cell i,j"
                           ## could cache this (memoize) but here we go...
                           in.range <- function(x, r) x >= min(r) && x <= max(r)
                           index <- widgets$each(function(ind, key, value) {
                             if(is.na(value$i) || is.na(value$j))
                               return(FALSE)
                             in.range(i, value$i) & in.range(j, value$j)
                           })
                           if(any(index))
                             return(widgets$get_item(which(index)))
                           else
                             return(NULL)
                         },
                         get_items = function(i,j, ..., drop=TRUE) {
                           "Return widgets in indices i,j in a list. If a single one and drop=TRUE, then return the object itself."
                           ## find all widgets matching i,j return as list
                           ## assume we have both i, j
                           if(missing(i))
                             i <- unlist(widgets$each(function(ind, key, value) value$i))
                           if(missing(j))
                             j <- unlist(widgets$each(function(ind, key, value) value$j))
                           l <- list()
                           for(row in i) {
                             for(col in j) {
                               w <- .get_widget(row, col)
                               if(!is.null(w))
                                 l[[length(l) + 1]] <- list(widget=w, row=row, column=col)
                             }
                           }
                           if(drop && length(l) == 1)
                             return(l[[1]]$widget)
                           else
                             return(l)
                         }

))
