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

##' Main window constructor
##'
##' There can be more than one gwindow instance per script, but one is
##' special. This one is called without a \code{parent} object, which
##' otherwise is typically another \code{gwindow} instance. The
##' special window sets up the environment to store the callbacks
##' etc. Subwindows are possible. Simply pass a value of \code{NULL}
##' to the argument \code{renderTo}. This argument is used to specify
##' the DOM id of a \code{DIV} tag. If given, the GUI created by the
##' \code{gwindow} call will replace this part of the web page. If not
##' given, then a subwindow will be rendered.
##' %
##' The \code{visible<-} method can be used to recompute the layout. This is often useful as the last line of a script.
##' @param title Window title
##' @param parent One and only one gwindow per script should have no
##' parent specified. Otherwise, this should be a \code{gwindow}
##' instance.
##' @param handler Handler called when window is closed. (For subwindows only)
##' @param action action passed to handler
##' @param ... ignored
##' @param renderTo Where to render window. For subwindows, this should be NULL. For main windows, this can be a DOM id or left as NULL, in which case the entire web page is used.
##' @param width width of a subwindow in pixels. 
##' @param height height of a subwindow in pixels
##' @param ext.args extra args passed to the constructor
##' @return An ExtContainer object
##' @export
##' @examples
##' w <- gwindow("Top level", renderTo="replaceme") ## no parent, so main one
##' g <- ggroup(cont=w)
##' b <- gbutton("click me for a subwindow", cont=g, handler=function(h,...) {
##'   w1 <- gwindow("subwindow -- no renderTo", renderTo=NULL, parent=w)
##'   g <- ggroup(cont=w1)
##'   gbutton("dispose", cont=g, handler=function(h,...) dispose(w1))
##' })
##' w2 <- gwindow("render elsewhere", parent=w, renderTo="replacemetoo") ## renderst to part of page
gwindow <- function(title="",
                    parent=NULL,
                    handler=NULL,
                    action=NULL,
                    ...,
                    renderTo=NULL, # or some DIV,
                    width=NULL,
                    height=NULL,
                    ext.args = NULL
                    ) {

  ## The gwindow object is a little special, as here we have to create
  ## the toplevel instance when it is the main window, as defined by
  ## not having a parent specified. (The parent holds the toplevel)
  if(is.null(parent)) {
    if(exists(".gWidgets_toplevel", inherits=TRUE)) {
      toplevel <- get(".gWidgets_toplevel", inherits=TRUE)
    } else {
      message("DEBUG only\n")
      toplevel <- GWidgetsTopLevel$new()
    }
    w <- GWindow$new(toplevel=toplevel)
   } else {
     if(!is.null(renderTo))
       w <- GWindow$new(parent=parent)
     else
       w <- GSubWindow$new(parent=parent)
  }
 
  w$init(title, parent, handler, action, ...,
         renderTo=renderTo,
         width=width, height=height, ext.args=ext.args)
  return(w)
}

##' base class for top-level windows and subwindows
##'
##' The \code{GWindow} class is used for windows and
##' subwindows. Windows in \pkg{gWidgetsWWW2} are rendered to parts of
##' the web page. In the simplest case, they are rendered to the
##' document body and are the only thing the user sees. However, one
##' can render to parts of a window as well. This is why we have a
##' \code{renderTo} argument in the constructor.
##'
##' One of the instances on a page contains the "toplevel" object,
##' which routes handler requests and gives web page responses.
##'
##' Subwindows are floating windows that appear on top of the web
##' page, like a dialog box.
##'
##' 
##' The method \code{start_comet} will launch a long-poll process,
##' whereby the browser repeatedly queries the server for any
##' changes. This can be useful if one expects to launch a
##' long-running process and the handler that initiates this will time
##' out before the process is done. One needs only to add the
##' javascript commands to the queue.
##' @rdname gwindow
GWindow <- setRefClass("GWindow",
                       contains="GContainer",
                       fields=list(
                         fullscreen="logical", # using a viewport?
                         loadmask_created ="logical"
                         ),
                       methods=list(
                         init = function(
                           title="",
                           parent=NULL,
                           handler=NULL,
                           action=NULL,
                           ...,
                           renderTo=NULL, # or some DIV,
                           width=NULL,
                           height=NULL,
                           ext.args = NULL
                           ) {

                        
                           ## we have two arguments: parent and renderTo
                           ## parent: if NULL, grab toplevel from environment. This is set once per page.
                           ## otherwise this is grabbed from the toplevel property of the parent object
                           
                           ## renderTo: This by default is Ext.getBody(), but could be name of a <div> tag, say
                           ## in which case the window will render within that tag. This allows one to work within
                           ## a web template, rather than old gWidgetsWW which was on script, one page



                           if(is.null(renderTo)) {
                             ## render to a ext.getBody and use a Viewport containing a panel
                             fullscreen <<- TRUE
                                                          
                             ## Hence this gets a little tricky
                             constructor <<- "Ext.container.Viewport"
                             arg_list <- list(
                                              layout="fit", ## fit
                                              renderTo=String("Ext.getBody()"),
                                              items=String(sprintf("[%s]",
                                                toJSObject(list(
                                                                xtype="panel",
                                                                id=get_id(),
                                                                layout="fit"
                                                                )
                                                           ))),
                                              ## items=String(paste("[{xtype:'panel',",,
                                              ##   sprintf("id:'%s'}]", get_id()),
                                              ##   sep="")),
                                              defaults=list(
                                                autoScroll=TRUE,
                                                autoHeight=TRUE,
                                                autoWidth=TRUE
                                                )
                                              )
                           } else if(!is.null(renderTo)) {
                             DEBUG("render to", renderTo)
                             constructor <<- "Ext.Panel"
                             fullscreen <<- FALSE
                             arg_list <- list(
                                              layout="fit",
                                              renderTo=renderTo,
                                              border=TRUE
                                              )
                           }
                           
                           add_args(arg_list, ext.args)
                           write_constructor()
                           
                           ## now steal object IDs if fullscreen
                           if(fullscreen) {
                             add_js_queue(sprintf("var %s_toplevel=%s;", get_id(), get_id()))
                             add_js_queue(sprintf("var %s=%s.child(0);", get_id(), get_id()))
                           }
                           
                             
                           if(!is.null(handler))
                             add_handler_changed(handler, action)
                           
                           ## write title
                           if(nchar(title))
                             set_value(title)
                           
                           loadmask_created <<- FALSE
                           
                           .self$toplevel$do_layout_cmd = sprintf("%s.doLayout()", get_id())
                           
                           callSuper(toplevel=.self)
                         },
                         ##
                         
                         add = function (child, expand, anchor, fill, ...) {
                           ## We override the sizing here
                           if(is(child, "GGroup") && getWithDefault(expand, TRUE)) {
                              cmd <- paste(child$get_id(),".setSize({width:'100%', height:'100%'});",sep="")
#                              add_js_queue(cmd)
                           }
                           callSuper(child, expand, anchor, fill, ...)
                         },
                         set_visible = function(bool) {
                           "For gwindow, call doLayout if TRUE"
                            do_layout()
                         },
                         ##
                         get_value = function(...) {
                           "Return title text"
                           value
                         },
                         set_value = function(text, ...) {
                           "Set title text"
                           value <<- text
                           add_js_queue(sprintf("document.title=%s;", ourQuote(text)))
                         },
                         dispose = function() {
                           call_Ext("hide")
                         },
                        
                         ##
                         do_layout=function() {
                           add_js_queue(sprintf("%s_toplevel.doLayout()", get_id()))
                         },
                         ##
                         cookies=function() {
                           toplevel$req$cookies()
                         },
                         ##
                         start_comet=function() {
                         "Turn on long-poll process for passing in commands from server"
                           add_js_queue("listen()")
                         },
                         ##
                         ## debugging
                         ##
                         dump = function() {
                           "Display js_queue for debugging"
                           toplevel$js_queue$core()
                         },
                         add_handler_destroy = function(handler, action=NULL) {
                           "When window is destroyed, this is called"
                           add_handler("destroy", handler, action)
                         }
                         ))
                       


GSubWindow <- setRefClass("GSubWindow",
contains="GWindow",
methods=list(
init = function(title, parent, handler, action, ..., renderTo=NULL,width=NULL, height=NULL, ext.args=list()) {
                           ## initialize a subwindow
                           width <- getWithDefault(width, 200)
                           height <- getWithDefault(height, 200)

                           constructor <<- "Ext.window.Window"
                           arg_list <- list(renderTo=String("Ext.getBody()"),
                                             title=title,
                                             layout="fit",
                                             width=width,
                                             height=height,
                                             closeAction="hide",
#                                             autoScroll=TRUE,
                                             plain=TRUE,
                                             button=String(sprintf('[{text: "Close", handler: function(){%s.hide()}}]',
                                               get_id()))
                                             )
                           add_args(arg_list, ext.args)

                           write_constructor()
                           
                           if(!is.null(handler))
                             add_handler_changed(handler, action)

#                           call_Ext("render")
                           call_Ext("show")
                           
                         }
))
