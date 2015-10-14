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

##' @include gframe.R
NA

##' gexpandgroup A box container which can disclose or hide its
##' children through a trigger icon.
##'
##' Use \code{svalue<-} to adjust the title. The \code{visible<-}
##' method is used to programatically change whether the child
##' components are displayed.
##' @param text label text
##' @param handler  Called when expanded or closed
##' @param action passed to handler
##' @param horizontal Boolean. Set to \code{TRUE} to pack children horizontally.
##' @param spacing Integer. Between child spacing.
##' @param use.scrollwindow Boolean. Set to \code{TRUE} to add a
##' scrollwindow to manage space given to child widgets. Containers
##' with scrollwindows often have their size fixed.
##' @inheritParams gwidget
##' @seealso \code{\link{ggroup}}, \code{\link{gframe}}
##' @export
##' @examples
##' w <- gwindow()
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
##' g <- gexpandgroup("click to close", cont=w)
##' gbutton("some button", cont=g)
##' visible(g) <- FALSE ## to close
gexpandgroup <- function(text = "", horizontal = TRUE,
                         spacing=2,
                         use.scrollwindow=FALSE,
                         container=NULL,
                         handler = NULL, action=NULL,
                         ...,
                         width=NULL,
                         height=NULL,
                         ext.args=NULL
                         ) {

  eg <- GExpandgroup$new(container, ...)
  eg$init(text=text,
          horizontal=horizontal,
          spacing=spacing,
          use.scrollwindow=FALSE,
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
                            contains="GFrame",
                            fields=list(
                              value="ANY",
                              visible="logical"
                              ),
                            methods=list(
                              set_visible = function(value, ...) {
                                "Show or collapse"
                                if(value)
                                  call_Ext("expand")
                                else
                                  call_Ext("collapse")
                                ..visible <<- as.logical(value)
                              }
                              ))
