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

##' @include gwidget-proxy.R
NULL

## These crash roxygen2, not sure why
## ## using a source
## email <- "ruser@gmail.com"
## u <- sprintf("http://www.gravatar.com/avatar/%s",digest(tolower(email)))
## ghtml(sprintf("<img src='%s' />", u), cont=g)
## ## using markdown package
## if(require(markdown)) {
## x <- "
## Header
## ======
##
## * item 1
## * item 2
##
## "
## ghtml(markdownToHTML(x), cont=g)
## }

##' widget to render HTML text pages
##'
##' 
##' @param x an HTML string or a URL. 
##' @param container parent container
##' @param ... passed to add method of parent container
##' @param width width of widge in pixels
##' @param height height of widget in pixels
##' @param ext.args extra arguments to pass to constructor
##' @return an \code{ExtWidget} object
##' @export
##' @examples
##' w <- gwindow()
##' g <- ggroup(cont=w, horizontal=FALSE)
##' h <- ghtml("<b>this is bold</b>", cont=g)
ghtml <- function(x, container = NULL,  ...,
                  width=NULL, height=NULL, ext.args=NULL
                  ) {
  h <- GHtml$new(container, ...)
  h$init(x, container, ...,
         width=width, height=height, ext.args=ext.args)
  h
}

GHtml <- setRefClass("GHtml",
                     contains="GWidget",
                     fields=list(
                       proxy = "ANY",
                       update_url = "list"
                       ),
                     methods=list(
                       init=function(x, container, ...,
                         width=NULL, height=NULL, ext.args=NULL) {

                         if(isURL(x)) {
                           x <- paste(readLines(x), collapse="")
                         }
                         
                         proxy <<- GWidgetHTMLProxy$new(container, ...)
                         proxy$init(x)
                         update_url <<- list(
                                             url=String("html_proxy_url"),
                                             method="GET",
                                             params=list(
                                               id=proxy$get_id(),
                                               session_id = String("session_id")
                                               ))
                         
                                          
                         constructor <<- "Ext.Panel"
                         arg_list = list(
                           autoLoad = update_url,
                           width=width,
                           height=height,
                           border=FALSE
                           )
                         add_args(arg_list)

                         setup(container, NULL, NULL, ext.args, ...)
                         
                       },
                       get_value = function(...) {
                         proxy$get_value()
                       },
                       set_value = function(value, ...) {
                         x <- value
                         if(isURL(x) ||
                            is(x[1], "StaticTempFile") ||
                            file.exists(x[1])) {
                           x <- readLines(x)
                         }
                         proxy$set_value(paste(x, collapse="\n"))

                         ## notify panel to update
#                         cmd <- sprintf("%s.update('%s', %s)", get_id(), x, toJSObject(update_url))
                         cmd <- sprintf("%s.getLoader().load();", get_id())
                         add_js_queue(cmd)
#                         parent$do_layout()
                       },
                       set_index=function(value, ...) {
                         set_value(value)
                       }
                       ))


##' Separator -- insert separator line
##'
##' Used in menus to create space. No container necessary
##' Can also be used to place horizontal  line using the \code{hr} HTML tag
##' 
##' @param horizontal Logical. Ignored, not vertical line possible
##' @param container parent container. Not used if creating menu or toolbar separator
##' @param ... passed to \code{add} method of parent container
##' @export
gseparator <- function(horizontal = TRUE, container = NULL, ...)  {
  if(is.null(container)) {
    s <- ""; class(s) <- "GSeparator"
    return(s)
  } else {
    ghtml("<hr />", container=container)
  }
}

