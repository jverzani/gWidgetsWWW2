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

##' File selection form
##'
##' This renders a file selection form within a small panel
##' widget. There are two steps needed to transfer a file: a) select a
##' file through a dialog, b) save the file to the server by clicking
##' the upload button.
##' @param text Instructional text. 
##' @param type only "open" implemented
##' @param filter ignored
##' @inheritParams gwidget
##' @return a \code{GFile} instance
##' @note the \code{svalue} method returns the temporary filename of
##' the uploaded file, or a value of \code{NA}. The property
##' \code{filename} holds the original filename.
##' @export
##' @examples
##' w <- gwindow()
##' gstatusbar("Powered by gWidgetsWWW2 and Rook", cont=w)
##' f <- gfile("Choose a file for upload", cont=w, handler=function(h,...) {
##'   galert(paste("You uploaded", svalue(h$obj)), parent=w)
##' })
gfile <- function(text="Choose a file",
                  type = c("open"),
                  filter = NULL, 
                  handler = NULL, action = NULL, container = NULL, ...,
                  width=NULL, height=NULL, ext.args=NULL
                  ) {
  f <- GFile$new(container$toplevel)
  f$init(text, type, filter, handler, action, container, ...,
         width=width, height=height, ext.args=ext.args)
  f
}

##' base class for gfile
##' @name gfile-class
GFile <- setRefClass("GFile",
                     contains="GWidget",
                     fields=list(
                       filename="character" # value is place, this is name
                       ),
                     method=list(
                       init=function(text, type, filter, handler, action, container, ...,
                         width=width, height=height, ext.args=ext.args) {

                         ## XXX handler?
                         constructor <<- "Ext.form.FormPanel"
                         arg_list <- list(
                                          width=width,
                                          height=height,
                                          frame=TRUE,
                                          fileUpload=TRUE,
                                          bodyPadding="2 2 0",
                                          url= String("file_url"),
                                          method="POST",
                                          defaults = list(
                                            anchor="100%",
                                            allowBlank=FALSE
                                            ),
                                          items=String(sprintf("[%s]",
                                            toJSObject(list(xtype="fileuploadfield",
                                                            id=sprintf("%s_upload", get_id()),
                                                            emptyText =text,
                                                            hideLabel = TRUE,
                                                            buttonText="Browse...",
                                                            buttonConfig=list(iconClass="update-icon")
                                                            )))),
                                          buttons = String(sprintf("[%s]",
                                            toJSObject(list(text=sprintf("%s",gettext("Upload")),
                                                            handler= String(paste("function () {",
                                                              sprintf("var form = %s.getForm();",get_id()),
                                                              "if(form.isValid()){",
                                                              "form.submit(",
                                                              toJSObject(list(
                                                                              waitMsg="Uploading...",
                                                                              success=String(sprintf("function(fp,o) {callRhandler('%s','%s')}", get_id(), "fileuploaded")),
                                                                              failure=String(sprintf("function(fp,o) {alert('')}", gettext("Upload failed"))),
                                                                              params=list(
                                                                                id=get_id(),
                                                                                session_id=String("session_id")
                                                                                )
                                                                              )),
                                                              ")}}",
                                                              sep=" "))))
                                                                                
                                                                      
                                            ))
                                          )
                
                         add_args(arg_list)

                         setup(container, NULL, NULL, ext.args, ...)
                         add_handler("fileuploaded", handler, action)

                         
                         set_value(NA)
                       },
                       set_value = function(value) {
                         "Set local file name. NA if not"
                         value <<- value
                       },
                       set_filename=function(fname) {
                         filename <<- fname
                       },
                       add_R_callback=function(...) {}

                       ))
                         
                                          
