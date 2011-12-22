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

##' @include ggroup.R
NA

##' gexpandgroup is a group with trigger icon and label
##'
##' Use \code{svalue<-} to adjust the title. The \code{visible<-}
##' method is used to programatically change display
##' @param text label text
##' @param handler  Called when expanded or closed
##' @param action passed to handler
##' @inheritParams ggroup
##' @seealso \code{\link{ggroup}}, \code{\link{gframe}}
##' @export
##' @examples
##' w <- gwindow()
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
##' g <- gexpandgroup("click to close", cont=w)
##' gbutton("some button", cont=g)
##' visible(g) <- FALSE ## to close
gexpandgroup <- function(text = "", horizontal = TRUE,
                         spacing=5,
                         handler = NULL, action=NULL,
                         container=NULL, ...,
                         width=NULL,
                         height=NULL,
                         ext.args=NULL
                         ) {

  eg <- GExpandgroup$new(container, ...)
  eg$init(horizontal=horizontal,
          spacing=spacing,
          use.scrollwindow = FALSE,
          container,
          ...,
          width=width,
          height=height,
          ext.args=merge.list(list(title=text,collapsible=TRUE),
            ext.args)
          )

  if(!is.null(handler)) {
    eg$add_handler("collapse", handler, action)
    eg$add_handler("expand", handler, action)
  }
  
  eg
}

GExpandgroup <- setRefClass("GExpandgroup",
                            contains="GGroup",
                            fields=list(
                              value="ANY",
                              visible="logical"
                              ),
                            methods=list(
                              set_value = function(value, ...) {
                                value <<- value
                                call_Ext("setTitle", value)
                              },
                              set_visible = function(value, ...) {
                                "Show or collapse"
                                if(value)
                                  call_Ext("expand")
                                else
                                  call_Ext("collapse")
                              }
                              ))
