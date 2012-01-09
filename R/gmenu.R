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

##' @include gwidget.R
NULL

##' Menubar implementation
##'
##' A menubar for gwindow instances
##' %
##' Menu items are specified with a named list. The heirarchical
##' nature of the list maps to the heirarchical menu structure, with
##' the names giving the menu names. The menu actions are specified
##' using \code{gaction} elements. These may also be
##' \code{gseperator()} instances (no parent necessary here).
##' %
##' Menubars should only be added to \code{gwindow} instances, but
##' this is not strictly enforced, any \code{Ext.Panel}-based
##' container would do.
##' @param menulist list of actions. Actions must have parent specified
##' @param popup Logical. ignored for now
##' @inheritParams gwidget
##' @return an ExtWidget object
##' @export
##' @examples
##' w <- gwindow()
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
##' stub = function(...) galert("stub", parent=w)
##' l <- list(
##' File = list(open=gaction("Open", handler=stub, parent=w),
##'             new = gaction("New", handler=stub, parent=w),
##'             gseparator(),
##'             quit = gaction("Quit", handler=stub, parent=w)),
##' Edit = list(save = gaction("Save", handler=stub, parent=w))
##' )
##' m <- gmenu(l, cont=w)
##' gtext("Text goes here", cont=w)
gmenu <- function(menulist,  popup = FALSE, action=NULL, container = NULL,..., ext.args=NULL) {
  m <- GMenu$new(container, ...)
  m$init(menulist, action, container, ..., ext.args=ext.args)
  m
}

##' base class for menu instances.
##' @name gmenu-class
GMenu <- setRefClass("GMenu",
                     contains="GContainer",
                     method=list(
                       init=function(menulist, action=NULL, container, ..., ext.args=NULL) {
                         constructor <<- "Ext.toolbar.Toolbar"
                         arg_list=list(
                           dock="top"
                           )
                         add_args(arg_list, ext.args)
                         write_constructor()
                         container$add_toolbar(.self)
                         add_to_toolbar(menulist)
                         container$do_layout()
                       },
                       add_to_toolbar=function(lst) {
                         addToToolbar(lst, nm="", self=.self)
                       },
                       remove_from_toolbar=function(obj) {
                         "Remove object from toolbar"
                         add_js_queue(sprintf("%s.remove(%s)",
                                              toolbar_id(), obj$get_id()))
                       },
                       toolbar_id=function() {
                         sprintf("%s_toolbar", get_id())
                       }
                       ))



## Tedious bit to avoid defined athrowaway S3 method in a reference method, which gives lookup issues
## though not reproducible in a small example
addToToolbar <- function(x, nm, self) UseMethod("addToToolbar")
addToToolbar.list <- function(x, nm="", self) {
  ## If nm="" then a toplevel list, else a
  ## named submenu
  if(nchar(nm) > 0 && is.list(x)) {
    ## submenu
    menu <- GMenuItem$new(parent=self)
    menu$init(x, nm, self)
    self$add_js_queue(sprintf("%s.add({text: '%s', menu:%s});",  self$get_id(), nm, menu$get_id()))
  } else {
    ## toolbar like
    nms <- names(x)
    if(is.null(nms)) {
      sapply(x, addToToolbar, nm="", self=self)
    } else {
      for(i in seq_along(x))
        addToToolbar(x[[i]], nm=nms[i], self=self)
#      mapply(addToToolbar, x, nm=nms, self=self)
    }
  }
}
addToToolbar.GComponent <- function(x, nm, self) {
  self$add_js_queue(sprintf("%s.add(%s);", self$get_id(), x$get_id()))
}
addToToolbar.GSeparator <- function(x, nm, self) {
  self$add_js_queue(sprintf("%s.add('-');", self$get_id()))
}

## Class for menubar items
GMenuItem <- setRefClass("GMenuItem",
                         contains="GWidget",
                         methods=list(
                           init=function(lst, nm, self) {
                             
                             constructor <<- "Ext.menu.Menu"
                             arg_list <- list(
                                              floating=TRUE,
                                              plain=TRUE
                                              
                                              )
                             add_args(arg_list)
                             write_constructor()

                             add_menu_items(lst)
                           },
                           add_menu_items=function(lst) {
                             nms <- names(lst)
                             has_names <- !is.null(nms)

                             for(i in seq_along(lst)) {
                               item <- lst[[i]]
                               if(is(item, "list")) {
                                 ## recurse?
                               } else if(is(item, "GAction")) {
                                 cmd <- sprintf("%s.add(%s);", get_id(), item$get_id())
                                 add_js_queue(cmd)
                               } else if(is(item, "GSeparator")) {
                                 add_js_queue(sprintf("%s.add('-');", get_id()))
                               } else {
                                 cmd <- sprintf("%s.add(%s);", get_id(), item$get_id())
                                 add_js_queue(cmd)
                               }
                             }
                           }
                              
                           ))
