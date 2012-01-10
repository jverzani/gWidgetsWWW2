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
##' will occur. One can specify i and j as a range of values,
##' or even empty
##'
##' The layout is only finalized after call the \code{visible<-}
##' method with a value of \code{TRUE}. One adds all the desired
##' children, then calls this method. It is at this point the widget
##' is rendered, so if the widget is added to a box container, say, to
##' which other components are added this one will appear after any
##' others that had been added. (So pretty much, add the children then
##' call visible.)
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
##' tbl[3,2] <- gbutton("click", cont=tbl, handler=function(h,...) {
##'   msg <- whisker.render("{{name}} is a {{rank}}",
##'            setNames(lapply(tbl[,2], svalue)[1:2], c("name", "rank")))
##'   galert(msg, parent=w)
##' })
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
                         container="ANY",
                         dots="list"
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
                           dots <<- list(...)
                           
                           constructor <<- "Ext.Panel"
                           arg_list <- list(
                                            defaults=list(
                                              bodyStyle = sprintf("padding:%spx;", spacing)
                                              )
                                            )
                           add_args(arg_list)

                           ## we write constructor in set_visible
                           ## and add to parent container
                         },
                         add=function(child, ...) {
                           "just keep track of bookkeeping"
                           child_bookkeeping(child)                           
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
                         mapAnchorToCSSClass = function(anchor) {
                           "Return a css class for the anchor value"
                           if(is.null(anchor))
                             return("td-northwest")
                           if(all(anchor == 0))
                             return("td-center")
                           
                           lr <- c("west", "", "east")
                           ns <- c("north", "", "south")
                           m <- rbind(paste("td", "-", ns[1], lr, sep=""),
                                      paste("td", "-", ns[2], lr, sep=""),
                                      paste("td", "-", ns[3], lr, sep="")
                                      )
                           
                           m[ 2 - anchor[2], 2 + anchor[1]]
                         },
                         set_visible = function(value, ...) {
                           if(!value) return()

                           ## Need a new algorithm:
                           ## for each row, loop over column
                           ## get widget in (i,j) configure colspan and rowspan from widget
                           ## if empty add blank label
                           f <- function(x, default) if(is.na(diff(range(x)))) default else 1 + diff(range(x))
                           already_seen <- Array$new()
                           for(i in 1:no_rows()) {
                             for(j in 1:no_columns()) {
                               cur_widget <- .get_widget(i,j)
                               if(is.null(cur_widget)) {
                                 label <- glabel("", cont=.self)
                                 already_seen$push(list(widget=label, i=i, j=j))
                               } else {
                                 if(!already_seen$contains_item(cur_widget)) {
                                    rowspan <- f(cur_widget$i, no_rows())
                                    colspan <- f(cur_widget$j, no_columns())
                                    anchorCls <- mapAnchorToCSSClass(cur_widget$anchor)

                                    ## add row and column span attributes to child widget etc:
                                    l <- list(rowspan=rowspan,
                                              colspan = colspan,
                                              cellCls = anchorCls,
                                              style=list(paddding=sprintf("%spx", spacing))
                                              )
                                    cur_widget$widget$ext_apply(l)

                                    already_seen$push(cur_widget)
                                  }
                               }
                             }
                           }
                           
                           
                           ## now we add more arguments, then write constructor
                           arg_list <- list(layout=list(type="table",
                                              columns=no_columns()
                                              ),
                                            items=String(sprintf("[%s]",
                                              paste(sapply(already_seen$core(), function(a) a$widget$get_id()), collapse=",")
                                            ))
                                            )
                           add_args(arg_list)
                           write_constructor()
                           ## funny way to add, but need to pass in
                           ## expand, fill, anchor arg when adding to
                           ## parent
                           dots$child <<- .self
                           do.call(container$add, dots)
                         },
                         do_layout=function(...) {
                           ## XXX How to refresh layout?
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
                         get_dim = function() c(rows=no_rows(), columns=no_columns()),
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
                           "Return widgets in indices i,j. NON-standard return: if i,j single values return widget, if row or column, return list, else return matrix of widgets"

                           if(missing(i) && missing(j)) {
                             i <- seq_len(no_rows())
                             j <- seq_len(no_columns())
                             get_items(i,j, ..., drop=drop) 
                           } else if(missing(i)) {
                             i <- seq_len(no_rows())
                             if(length(j) == 1) {
                               return(lapply(i, get_items, j=j))
                             } else {
                               get_items(i, j, ..., drop=drop)
                             }
                           } else if(missing(j)) {
                             j <- seq_len(no_columns())
                             if(length(i) == 1) {
                               ## return list
                               return(lapply(j, get_items, i=i))                               
                             } else {
                               get_items(i, j, ..., drop=drop)
                             }
                           } else {
                             if(length(i) == 1 && length(j) == 1) {
                               return(.get_widget(i,j)$widget)
                             } else {
                               sapply(j, function(col) lapply(i, get_items, j=col))
                             }
                           }
                         }
                         
))
