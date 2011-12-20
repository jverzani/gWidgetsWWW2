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

##' Container for an image
##'
##' The image shows a url. This url can be external. For local urls,
##' the file must be accessible by the server. The convenience method
##' \code{obj$get_tempfile()} or the function \code{get_tempfile} can
##' be used to create a file that is accessible. See the example.
##' @param filename A url or file in static temp file
##' @param dirname prepended to filename if non empty
##' @param size passed to \code{size<-} method if given
##' @param handler ignored
##' @param action ignored
##' @param container parent container
##' @param ... passed to parent container's \code{add} method
##' @param width width in pixels
##' @param height height in pixels
##' @param ext.args extra arguments to pass to Ext constructor
##' @return an ExtWidget object
##' @export
##' @examples
##' w <- gwindow("hello", renderTo="replaceme")
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
##' g <- ggroup(cont=w, horizontal=FALSE)
##' 
##' f <- get_tempfile(ext=".png")
##' png(f)
##' hist(rnorm(100))
##' dev.off()
##' 
##' i <- gimage(f, container=g)
##' b <- gbutton("click", cont=g, handler=function(h,...) {
##'   f <- i$get_tempfile(ext=".png")   ## a method call, for variety
##'   png(f)
##'   hist(rnorm(100))
##'   dev.off()
##'   svalue(i) <- f
##' })
##' @note requires tempdir to be mapped to a specific url, as this is
##' assumed by \code{get_tempfile} and \code{get_tempfile_url}
gimage <- function(filename = "", dirname = "",  size = "",
                   handler = NULL, action = NULL, container = NULL,...,
                   width=NULL, height=NULL, ext.args=NULL
                   ) {
  i <- GImage$new(container, ...)
  i$init(filename, container, ...,
         width=width, height=height, ext.args=ext.args)
  i
}

##' base class for gimage
##' @name gimage-class
GImage <- setRefClass("GImage",
                      contains="GHtml",
                      method=list(
                        init=function(filename, container, ...,
                          width=NULL, height=NULL, ext.args=NULL) {

                          callSuper(img_wrap(filename), container, ...,
                                    width=width, height=height, ext.args=ext.args)
                        },
                        img_wrap =function(x, alt="") {
                          if(missing(x)) {
                            value <<- ""
                          } else if(is(x, "StaticTempFile")) {
                            value <<- get_tempfile_url(x)
                          } else {
                            value <<- x # assume a url
                          }
                          sprintf("<img src=\"%s\" alt=\"%s\" />", x, alt)
                        },
                        set_value = function(f, alt="", ...) {
                          x <- img_wrap(f, alt=alt)
                          callSuper(x)
                        },
                        get_tempfile = function(ext=".txt") {
                          "Create a new temporary file"
                          get_tempfile(ext)
                        }
                        ))

                           

