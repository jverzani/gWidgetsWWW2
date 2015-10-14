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
NA

##' Status bar for gwindow instances
##'
##' Status for main window. Use \code{gwindow} instance for parent
##' container. The \code{svalue<-} method can be used to change the
##' value.
##' @param text text for label
##' @inheritParams gwidget
##' @return an ExtWidget instance
##' @export
##' @examples
##' w <- gwindow()
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook")
gstatusbar <- function(text = "", container=NULL, ..., ext.args=NULL) {
  if(!is(container, "GWindow") || is(container, "GGroup"))
    return()

  sb <- GStatusbar$new(container, ...)
  sb$init(text, container, ..., ext.args=ext.args)
  sb
}

##' \code{GStatusbar}  class for gstatusbar
##'
##' The convenience reference class method \code{clear} will clear the
##' text. This is basically the same as \code{svalue(obj) <- ""}.
##'
##' We add in reference methods \code{show_loading} and
##' \code{hide_loading} to allow one to indicate something is
##' loading. Not really that useful though, as is. This is due to how
##' the JavaScript commands come back from the R process in a block so
##' unless the browser takes a long time to process the returning
##' JavaScript, you will likely show the loading for just a bit.
##'
##' This class inherits for \code{GContainer}, thereby allowing one to
##' add in widgets in addition to the message by using the status bar
##' as the parent container. This might be useful.
##' @name gstatusbar-class
GStatusbar <- setRefClass("GStatusbar",
                          contains="GContainer",
                          fields=list(
                            container="ANY"
                            ),
                          methods=list(
                            init=function(text, container, ..., ext.args=NULL) {
                              constructor <<- "Ext.toolbar.Toolbar"
                              value <<- text
                              tpl <- "
[{xtype:'label',text:''},
 {xtype:'label', id:'{{id}}_status', text:'{{txt}}'}
]
"
                              
                              arg_list=list(
                                dock='bottom',
                                items=String(whisker.render(tpl,list(id=get_id(),
                                                                     txt=escapeSingleQuote(text))))
                                )
                              add_args(arg_list, ext.args)
                              write_constructor()
                              container$add_statusbar(.self)
                            },
                            set_value = function(value, ...) {
                              cmd <- sprintf("%s.getComponent(1).setText('%s')",
                                             get_id(), value)
                              add_js_queue(cmd)
                              value <<- value
                            },
                            get_value = function(...) value,
                            clear = function() {
                              set_value("")
                            },
                            show_loading=function(msg="") {
                               if(!is.null(getOption("gWidgetsWWW2:FastRWeb")))
                                 url <- "/cgi-bin/R/gWidgetsWWW2?name=images/ajax-loader.gif"
                               else
                                 url <- "/custom/gWidgetsWWW2/images/ajax-loader.gif"
                               tpl <- "
{{id}}.getComponent(0).setText('<img src=\"{{url}}\" width=16 height=16 />{{msg}}', false);
{{id}}.doLayout();
"
                            cmd <- whisker.render(tpl, list(url=url,id=get_id(), msg=escapeSingleQuote(msg)))
                            add_js_queue(cmd)
                            },
                            hide_loading=function() {
                               tpl <- "
{{id}}.getComponent(0).setText('');
{{id}}.doLayout();
"
                            cmd <- whisker.render(tpl, list(id=get_id()))
                            add_js_queue(cmd)
                            }
                            ))
