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
      cat("DEBUG only\n")
      toplevel <- GWidgetsTopLevel$new()
    }
   } else {
    toplevel <- parent$toplevel
  }
  
  w <- GWindow$new(toplevel=toplevel)
  w$init(title, parent, handler, action, ...,
         renderTo=renderTo,
         width=width, height=height, ext.args=ext.args)
  return(w)
}

##' base class for top-level windows and subwindows
##' @name gwindow-class
##' Method \code{start_comet} will launch long-poll process
GWindow <- setRefClass("GWindow",
                       contains="GContainer",
                       fields=list(
                         fullscreen="logical" # using a viewport?
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

                           constructor <<- "Ext.Panel"
                           fullscreen <<- FALSE

                           ## we have two arguments: parent and renderTo
                           ## parent: if NULL, grab toplevel from environment. This is set once per page.
                           ## otherwise this is grabbed from the toplevel property of the parent object
                           
                           ## renderTo: This by default is Ext.getBody(), but could be name of a <div> tag, say
                           ## in which case the window will render within that tag. This allows one to work within
                           ## a web template, rather than old gWidgetsWW which was on script, one page



                           arg_list <- list(
                                            layout="fit"
                                            )

                           ## Now what...
                           if(is.null(parent)) {
                             if(is.null(renderTo)) {
                               ## render to a ext.getBody and use a Viewport containing a panel
                               ## Hence this gets a little tricky
                               constructor <<- "Ext.container.Viewport"
                               fullscreen <<- TRUE
                               arg_list[['renderTo']] <- String("Ext.getBody()")
                               tpl <- "
[{xtype:'panel',
  dockedItems: %s,
  id:'%s_panel'
}]
"
                               arg_list[['items']] <- String(sprintf(tpl, docked_items(), get_id()))

                             } else if(!is.null(renderTo)) {
                               arg_list[['renderTo']] <- renderTo
                               arg_list[['border']] <- TRUE
                               arg_list[['hideBorders']] <- FALSE
                               arg_list[['dockedItems']] <- docked_items()
                             }

                             add_args(arg_list)
                             
                             if(!is.null(ext.args))
                               args$extend(ext.args)
                             
                             write_constructor()
                             
                             ## now steal object IDs if fullscreen
                             if(fullscreen) {
                               add_js_queue(sprintf("var %s_toplevel=%s;", get_id(), get_id()))
                               add_js_queue(sprintf("var %s=%s.child(0);", get_id(), get_id()))
                             }
                             ## create statusbar and toolbar IDs
                             add_js_queue(sprintf("var %s=%s.getComponent('%s_status_bar')",
                               status_id(), get_id(), get_id()))
                             add_js_queue(sprintf("var %s=%s.getComponent('%s_toolbar')",
                               toolbar_id(), get_id(), get_id()))
                             
                             
                             if(!is.null(handler))
                               add_handler_changed(handler, action)
                             
                             ## write title
                             if(nchar(title))
                               set_value(title)
                             
                             
                             .self$toplevel$do_layout_cmd = sprintf("%s.doLayout()", get_id())

                           } else {
                             ## do a subwindow
                             init_subwindow(title, parent, handler, action, ..., width=width, height=height)
                           }
                           
                           callSuper(toplevel=.self)
                         },
                         ##
                         init_subwindow = function(title, parent, handler, action, ..., width=NULL, height=NULL) {
                           ## initialize a subwindow
                           width <- getWithDefault(width, 200)
                           height <- getWithDefault(height, 200)

                           constructor <<- "Ext.Window"
                           arg_list <- list(renderTo=String("Ext.getBody()"),
                                             title=title,
                                             layout="auto",
                                             width=width,
                                             height=height,
                                             closeAction="hide",
                                             autoScroll=TRUE,
                                             plain=TRUE,
                                             button=String(sprintf('[{text: "Close", handler: function(){%s.hide()}}]',
                                               get_id()))
                                             )
                           add_args(arg_list)
                           write_constructor()
                           
                           if(!is.null(handler))
                             add_handler_changed(handler, action)

                           call_Ext("render")
                           call_Ext("show")
                           
                         },
                         set_visible = function(bool) {
                           "For gwindow, call doLayout if TRUE"
                            do_layout()
                         },
                         ##
                         get_value = function() {
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
                         ## XXX This all needs changing
                         add_handler_changed = function(handler, action=NULL) {
                           add_R_handler("onunload", handler, action)
                         },
                         add_handler_destroy = function(handler, action=NULL) {
                           "When window is destroyed, this is called"
                           cbid <- toplevel$add_R_handler(.self, handler, action=action)
                           cmd <- sprintf("window.onunload = function() {callRhandler(%s)};",
                                          cbid)
                           add_js_queue(cmd)
                           cbid
                         }
                         )
                       )
