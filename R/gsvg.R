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

##' widget to display svg files
##'
##' This widget displays an svg file, such as is made by the
##' \code{svg} driver or the \code{devSVGTips} driver in the
##' \pkg{RSVGTipsDevice}. The basic usage is like \code{gimage}, where
##' a file that is stored in a place the web browser can serve is used
##' as the file specified to the device. The convenience method
##' \code{get_tempfile} (and function) is used to create such a
##' file. Use the \code{.svg} extension. The widget size is specified
##' in pixels, but the svg device driver is in inches.
##' @param f filename. Usually produced by \code{get_tempfile(ext=".svg")}.
##' @param width width of widget in pixels
##' @param height height of widget in pixels
##' @param container parent container
##' @param ... passed to \code{add} method of parent container
##' @param ext.args Means to pass additional arguments to Ext constructor
##' @return An ExtWidget instance
##' @export
##' @examples
##' \dontrun{
##' w <- gwindow("hello", renderTo="replaceme")
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
##' g <- ggroup(cont=w, horizontal=FALSE)
##' require(RSVGTipsDevice)
##' 
##' f <- get_tempfile(ext=".svg") ## use this extension
##' svg(f)
##' hist(rnorm(100))
##' dev.off()
##' 
##' i <- gsvg(f, container=g, width=480, height=480)
##' 
##' 
##' 
##' b <- gbutton("click", cont=g, handler=function(h,...) {
##'   f <- get_tempfile(ext=".svg")
##'   svg(f)
##'   hist(rnorm(100))
##'   dev.off()
##'   svalue(i) <- f
##' })
##' }
gsvg <- function(f, width=480, height=400,
                 container = NULL,..., ext.args=NULL) {

  gs <- GSvg$new(container$toplevel)
  gs$init(f, width, height, container, ..., ext.args=ext.args)
  gs
}

##' base class for gsvg
##' @name gsvg-class
GSvg <- setRefClass("GSvg",
                    contains="GWidget",
                    fields=list(
                      width="numeric",
                      height="numeric"
                      ),
                    methods=list(
                      init=function(f, width=480, height=400, container, ..., ext.args=NULL) {

                        constructor <<- "Ext.Panel"
                        width <<- width
                        height <<- height
                        
                        arg_list=list(
                          width=width,
                          height=height,
                          border = FALSE,
                          html = sprintf("<div id='%s'></div>", get_svg_id())
                          )

                        add_args(arg_list)
                        setup(container, NULL, NULL, ext.args, ...)
                        
                        if(!missing(f))
                          set_value(f)
                        
                        
                      },
                      set_value = function(value, ...) {
                        "Set value, then call JS to display"
                        
                        if(is(value, "StaticTempFile"))
                            value <<- get_tempfile_url(value)
                          else
                            value <<- value # assume a url

                        cmd <- paste(sprintf("var tmp = document.getElementById('%s');", get_svg_id()),
                                     sprintf('tmp.innerHTML = "<embed src=\'%s\' width=%s height=%s type=\'image/svg+xml\'>;\"',
                                             .self$value,
                                             width, height),
                                     sep="")
                        add_js_queue(cmd)
                        },
                      get_svg_id = function() {
                        "Return id of svg div element"
                        sprintf("%s_svg", id)
                      },
                      get_tempfile = function(ext=".txt") {
                        "Create a new temporary file"
                        get_tempfile(ext)
                      }
                      ))
