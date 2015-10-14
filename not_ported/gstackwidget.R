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

##' stack widget is a "card" container. Use \code{gnotebook} methods to change cards
##'
##' @inheritParams gwidget
##' @return a GStackWidget reference class object. 
##' @export
##' @examples
##' w <- gwindow()
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
##' g <- gstackwidget(cont=w)
##' gbutton("page 1. Next", cont=g, handler=function(h,...) svalue(g) <- svalue(g) + 1)
##' gbutton("page 2. Next", cont=g, handler=function(h,...) svalue(g) <- svalue(g) + 1)
##' gbutton("remove me", cont=g, handler=function(h,...) dispose(g))
##' gbutton("page 3. First", cont=g, handler=function(h,...) svalue(g) <- 1)
##' svalue(g) <- 1
gstackwidget <- function(container, ...,
                         width=NULL, height=NULL, ext.args=NULL
                      ) {
  sw <- GStackWidget$new(container,...)
  sw$init(container, ..., width=width, height=height, ext.args=ext.args)
  sw
}

GStackWidget <- setRefClass("GStackWidget",
                         contains="GContainer",
                         fields=list(
                           "notebook_children"="list"
                           ),
                         methods=list(
                           init=function(container, ...,
                             width=NULL, height=NULL, ext.args=NULL) {

                             notebook_children <<- list()
                             constructor <<- "Ext.Panel"
                             value <<- 1 ## which card is visible (getActiveItem + 1)
                             
                             
                             arg_list <- list(layout="card",
                                              bodyStyle="padding:15px",
                                              width=width,
                                              height=height,
                                              defaults=list(border=FALSE)
                                              )

                             add_args(arg_list, ext.args)
                             write_constructor()
                             container$add(.self, ...)
                             
                             
                           },
                           process_transport = function(value) {
                             value <<- value
                           },
                           add = function(child, label="tab", tooltip=NULL, ...) {
                             "Add child. Label is tab label"

                             ## book keep
                             children$push(child, child$get_id())
                             notebook_children[[child$id]] <<- child

                             
                             call_Ext("add", list(
                                                  items=String(sprintf("['%s']", child$id))
                                                  ))

                             set_value(length(notebook_children))
                           },
                           dispose = function(index) {
                             "For deleting. Index can be numeric or character"

                             if(missing(index))
                               index <- value ## current one
                             notebook_children[[index]] <<- NULL
                             cmd <- whisker.render(dispose_template(),
                                                   list(id=get_id(),
                                                        page_no=index-1))
                             add_js_queue(cmd)
                             set_value(max(1, index - 1))
                           },
                           len = function() {
                             "Number of cards"
                             base:::length(notebook_children)
                           },
                           get_value = function(...) {
                             value
                           },
                           set_value = function(value, ...) {
                             "make tab value visible"
                             value <<- value
                             add_js_queue(whisker.render('{{id}}.getLayout().setActiveItem({{page_no}});', list(id=get_id(), page_no=value - 1L)))
                           },
                           next_card = function() call_Ext("next"),
                           previous_card = function() call_Ext("prev"),
                           ## templates
                           dispose_template=function() {
                             tpl <- "
var card = {{id}}.getComponent({{page_no}});
{{id}}.remove(card);
"
tpl
}

                           ))
                           
                        
