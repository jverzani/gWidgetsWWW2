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

## helper to format messasge with detail
.make_message <- function(msg) {
  if(length(msg) > 1)
    msg <- sprintf("<b>%s</b><br/>%s",
                   msg[1],
                   paste(msg[-1], collapse="<br/>")
                   )
  escapeSingleQuote(msg)
}

##' A simple message dialog.
##' 
##' @param message main message.
##' @param title Title for dialog's window
##' @param icon icon to decorate dialog. One of \code{c("info", "warning", "error", "question")}.
##' @param parent parent container (the main window instance)
##' @param ... ignored
##' @return return value ignored
##' @export
##' @examples
##' w <- gwindow()
##' gbutton("click me for a message", cont=w, handler=function(h,...) {
##' gmessage("Hello there", parent=w)
##' })
gmessage <- function(message, title="message",
                     icon = NULL,
                     parent = NULL,
                     ...) {

 
  
  dlg <- GDialog$new(parent$toplevel)
  icon <- sprintf("Ext.MessageBox.%s", toupper(match.arg(icon,c("info", "warning", "error", "question"))))

  tpl <- "
Ext.Msg.show({
  title:'{{title}}',
  msg:'{{message}}',
  buttons:Ext.Msg.CANCEL,
  icon:{{icon}},
  animEl:'{{parent_id}}'
});
"
  cmd <- whisker.render(tpl,
                        list(title=escapeSingleQuote(title),
                             message=.make_message(message),
                             icon=icon,
                             parent_id=parent$id)
                        )
  dlg$add_js_queue(cmd)
}


##' Confirmation dialog
##'
##' Calls handler when Yes button is clicked. Unlike other gWidgets
##' implementations, this one does not block the R process before
##' returning a logical indicating the selection. One must use a
##' handler to have interactivity.
##' @param message message
##' @param title title for dialog's window
##' @param icon icon. One of 'info', 'warning', 'error', or 'question'.
##' @param parent parent container (main window instance)
##' @param handler handler passed to dialog if confirmed
##' @param action passed to any handler
##' @param ... ignored
##' @return return value ignored, use handler for response.
##' @export
##' @examples
##' w <- gwindow()
##' gbutton("click me for a message", cont=w, handler=function(h,...) {
##' gconfirm("Do you like R", parent=w, handler=function(h,...) {
##' galert("Glad you do", parent=w)
##' })
##' })
gconfirm <- function(message, title="Confirm",
                     icon = NULL,
                     parent = NULL,
                     handler = NULL,
                     action = NULL,...) {

  dlg <- GDialog$new(parent$toplevel)
  dlg$change_signal <- "confirm"
  
  dlg$add_handler(dlg$change_signal, handler, action)
    
  tpl <- "
Ext.Msg.show({
  title:'{{title}}',
  msg:'{{message}}',
  buttons:Ext.Msg.YESNO,
  icon:Ext.MessageBox.{{icon}},
  animEl:'{{parent_id}}',
  fn:function(buttonID, text, o) {
    if(buttonID == 'yes') {
      callRhandler('{{id}}', '{{signal}}',null)
    }
  }
});
"
  cmd <- whisker.render(tpl,
                        list(title=escapeSingleQuote(title),
                             message=.make_message(message),
                             icon=toupper(match.arg(icon,c('info', 'warning', 'error', 'question'))),
                             parent_id=parent$id,
                             id=dlg$get_id(),
                             signal=dlg$change_signal
                             ))
  dlg$add_js_queue(cmd)
}



             
##' input dialog.
##'
##' Used for getting a text string to pass to a handler. Unlike other
##' gWidgets implementations, this call does not block the R process,
##' so any response to the user must be done through the handler.
##' @param message message
##' @param text initial text for the widget
##' @param title title for dialog's window
##' @param icon icon ignored here
##' @param parent parent container (main window instance)
##' @param handler Called if yes is selected, the component
##' \code{input} of the first argument holds the user-supplied string.
##' @param action passed to any handler
##' @param ... ignored
##' @return return value ignored, use handler for response
##' @export
##' @examples
##' w <- gwindow()
##' gbutton("click me for a message", cont=w, handler=function(h,...) {
##'   ginput("What is your name?", parent=w, handler=function(h,...) {
##'     galert(paste("Hello", h$input), parent=w)
##'   })
##' })
ginput <- function(message, text="", title="Input",
                   icon = NULL,
                   parent=NULL,
                   handler = NULL, action = NULL,...) {

  dlg <- GDialog$new(parent=parent)
  dlg$change_signal <- "input"          # means nothing to Ext
  dlg$add_handler(dlg$change_signal, handler, action)

  
  fn <- paste("function(buttonID, text) {",
              "if(buttonID == 'ok') {",
              sprintf("callRhandler('%s', '%s',  {input:text})",
                      dlg$get_id(),
                      dlg$change_signal
                      ),
              "}}",
              sep="")
                             
                                  
  cmd <- sprintf("Ext.Msg.prompt('%s','%s', %s, this, true, '%s');",
                 escapeSingleQuote(title),
                 .make_message(message),
                 fn,
                 escapeSingleQuote(text)
                 )
  dlg$add_js_queue(cmd)
}

##' Base class for dialogs
GDialog <- setRefClass("GDialog",
                       contains="GWidget",
                       methods=list(
                         ## bypass this
                         add_R_callback=function(...) {}
                         )

                       )




##' quick alert message -- not modal or obtrusive (dropped from above in extjs)
##' 
##' @param message message to display
##' @param title title of message
##' @param delay delay in seconds
##' @param parent parent window, typically gwindow instance. Necessary.
##' @return not used
##' @export
##' @examples
##' w <- gwindow()
##' b <- gbutton("click me", cont=w, handler=function(h,...) {
##' galert("That hurt", parent=w)
##' })
##' 
galert <- function(message, title = "message", delay=3, parent) {

  dlg <- GDialog$new(parent$toplevel)

  ## parent not used here
  if(missing(message))
    message <- ""
  
  cmd <- sprintf("Ext.example.msg('%s', '%s', %s);",
                 escapeSingleQuote(title),
                 .make_message(message),
                 delay)

  dlg$add_js_queue(cmd)
  
  
}

##' Stub for a basic dialog
##'
##' There is no gbasicdialog in \pkg{gWidgetsWWW2}. There is a
##' workaround though. In a subwindow pass in the argument
##' \code{ext.args=list(modal=TRUE)}. This will make the floating
##' subwindow modal. There is not need to call \code{visible} -- in
##' fact it wont work -- but you need to define the window and its
##' child components within a single call back.
##' @param ... ignored
##' @return NULL
##' @export
##' @examples
##' \dontrun{
##' w <- gwindow("Parent")
##' b <- gbutton("click", cont=w, handler=function(h,...) {
##'   w1 <- gwindow("modal subwindow", parent=w, ext.args=list(modal=TRUE))
##'   g <- ggroup(cont=w1, horizontal=FALSE)
##'   glabel("Some label", cont=g, expand=TRUE)
##'   bg <- ggroup(cont=g)
##'   addSpring(bg)
##'   gbutton("ok", cont=bg) ## add your own handler
##'   gbutton("dismiss", cont=bg, handler=function(...) dispose(w1))
##' })
##' }
gbasicdialog <- function(...) {

}
