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
##' the file must be accessible by the server. The function
##' \code{get_tempfile} can be used to create a file that is so
##' accessible. See the example.
##' @param filename A url or file from \code{get_tempfile}.
##' @param dirname ignored.
##' @param size A vector passed to \code{width} and \code{height} arguments.
##' Can also be set by the \code{size<-} method later.
##' @inheritParams gwidget
##' @return an GImage reference object
##' @export
##' @examples
##' w <- gwindow("hello")
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
##' g <- ggroup(cont=w, horizontal=FALSE)
##' 
##' f <- get_tempfile(ext=".png")
##' png(f)
##' hist(rnorm(100))
##' dev.off()
##' 
##' i <- gimage(f, container=g, size = c(400, 400))
##' b <- gbutton("click", cont=g, handler=function(h,...) {
##'   f <- get_tempfile(ext=".png") 
##'   png(f)
##'   hist(rnorm(100))
##'   dev.off()
##'   svalue(i) <- f
##' })
##' @note requires tempdir to be mapped to a specific url, as this is
##' assumed by \code{get_tempfile} and \code{get_tempfile_url}
gimage <- function(filename = "", dirname = "",  size = NULL,
                   handler = NULL, action = NULL, container = NULL,...,
                   width=NULL, height=NULL, ext.args=NULL
                   ) {


  
  i <- GImage$new(container, ...)
  i$init(filename, container, ...,
         width=width, height=height, ext.args=ext.args)
  if(!is.null(size))
    size(i) <- size
  i
}


GImage <- setRefClass("GImage",
                      contains="GHtml",
                      fields=list(
                        filename="ANY"
                        ),
                      method=list(
                        init=function(filename, container, ...,
                          width=NULL, height=NULL, ext.args=NULL) {

                          filename <<- filename
                          callSuper(img_wrap(filename), container, ...,
                                    width=width, height=height, ext.args=ext.args)
                        },
                        img_wrap =function(x, alt="") {
                          "wrap image file into HTML call"
                          if(missing(x)) {
                            value <<- ""
                          } else if(is(x, "StaticTempFile")) {
                            value <<- get_tempfile_url(x)
                          } else {
                            value <<- x # assume a url
                          }
                          sprintf("<img src=\"%s\" alt=\"%s\" />", value, alt)
                        },
                        set_value = function(f, alt="", ...) {
                          filename <<- filename
                          x <- img_wrap(f, alt=alt)
                          callSuper(x)
                        },
                        get_value = function(...) {
                          filename
                        },
                        get_index=function(...) {
                          get_value()
                        }
                        ))

                           

