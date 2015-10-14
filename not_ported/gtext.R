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

##' @include gwidget-text.R
NULL

##' A text area widget
##'
##' @param text initial text
##' @param width width in pixels
##' @param height height in pixels
##' @param font.attr Ignored. Default font attributes
##' @param wrap Ignored Do we wrap the tet
##' @inheritParams gwidget
##' @return an ExtWidget instance
##' @export
##' @examples
##' w <- gwindow("gtext example")
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
##' g <- ggroup(cont=w, horizontal=FALSE)
##' t <- gtext("Some text with \n new lines", cont=g)
##' b <- gbutton("click", cont=g, handler=function(h,...) {
##'   galert(svalue(b), parent=w)
##' })
##'  b <- gbutton("change", cont=g, handler=function(h,...) {
##'    svalue(t) <= "some new text"
##' })
gtext <- function(text = NULL, width = NULL, height = 300,
                  font.attr = NULL, wrap = TRUE,
                  handler = NULL, action = NULL, container = NULL,...,
                  ext.args=NULL
                  ) {

  if(getFromDots("use.codemirror", ..., default=FALSE))
  ## This seems too slow and sizing is awkward
    txt <- GCodeMirrorPlainText$new(container, ...)
  else    
    txt <- GText$new(container,...)
  txt$init(text, font.attr, wrap, handler, action, container, ...,
           width=width, height=height,
           ext.args=ext.args)
  txt
}

## TODO: ace editing? 
##       fonts

## base class for gtext
GText <- setRefClass("GText",
                     contains="GWidgetText",
                     method=list(
                       init = function(text, font.attr, wrap, handler, action, container, ...,
                         width, height, 
                         ext.args=list()) {

                         value <<- paste(getWithDefault(text, ""), collapse="\n")
                         constructor <<- "Ext.form.TextArea"
                         transport_signal <<- ifelse(is_running_local(), "change", "blur")
                         
                         arg_list <- list(value=escapeSingleQuote(.self$value),
                                          width=width,
                                          height=height,
                                          selectOnFocus = TRUE,
                                          enableKeyEvents=TRUE,
                                          fieldLabel=list(...)$label
                                          )

                         add_args(arg_list)
                         setup(container, handler, action, ext.args, ...)
                       },
                       insert = function(value, where, do.newline=TRUE) {
                         "Insert new text at ..."

                         ## we don't have an insert method so we add and change
                         if(where == "beginning")
                           newValue <- paste(value,
                                             .self$value,
                                             sep=ifelse(do.newline, "\n", ""))
                         else
                           newValue <- paste(.self$value,
                                             value,
                                             sep=ifelse(do.newline, "\n", ""))
                         value <<- newValue

                         call_Ext("setRawValue", value)

                       }
                       ))


##' A gtext object modified to use codemirror for syntax highlighting
##'
##' The CodeMirror project (codemirror.net) provides a code editor in
##' javascript with an R mode. This widget incorporates that into
##' \pkg{gWidgetsWWW2}. This does R syntax highlighting. To use
##' codemirror with gtext (and no highlighting), pass the argument
##' \code{use.codemirror=TRUE} to the \code{gtext} call.
##' @title codemirror 
##' @param text initial text.
##' @inheritParams gwidget
##' @return a GCodeMirror reference class
##' @export
##' @author john verzani
gcodemirror <- function(text="", container=NULL, ..., width=NULL, height=NULL, ext.args=list()) {
  obj <- GCodeMirror$new(container)
  obj$init(text, container, width=width, height=height, ext.args=ext.args, ...)
  obj
}

GCodeMirror <- setRefClass("GCodeMirror",
                           contains="GText",
                           methods=list(
                             init=function(text="", container, ..., width, height, ext.args) {
                               constructor <<- "Ext.ux.CodeMirror"
#                               transport_signal <<- "change"
                               
                               arg_list <- list(
                                                width=width,
                                                height=height,
                                                lineNumbers=TRUE,
                                                matchBrackets=TRUE,
                                                electricChars=TRUE,
                                                mode="r"
                                               
                                                )
                               arg_list <- add_on_callbacks(arg_list)
                               add_args(arg_list)
                               setup(container, NULL, NULL, ext.args, ...)
                               set_value(text)
                               add_public_method(c("process_transport"))
                             },
                             add_on_callbacks=function(lst) {
                               lst <- merge(lst,
                                            ##signals are focus, blur
                                            list(
                                                 onChange=String(paste(
                                                   sprintf("function(me, obj) {"),
                                                   sprintf("  jRpc('%s', 'process_transport', obj);", get_id()),
                                                   sprintf("}"),
                                                   sep="")),
                                                 onGutterClick=String("CodeMirror.newFoldFunction(CodeMirror.braceRangeFinder)"),
                                                 onFocus=String(paste(
                                                   sprintf("function() {"),
                                                   sprintf("callRhandler('%s','focus',null);", get_id()),
                                                   sprintf("}"),
                                                   sep="")),
                                                 onBlur=String(paste(
                                                   sprintf("function() {"),
                                                   sprintf("callRhandler('%s','blur',null);", get_id()),
                                                   sprintf("}"),
                                                   sep=""))
                                                 ))
                               return(lst)
                             },
                             get_value=function(...) {
                               "We store character vector of lines, so we need to paste on return"
                               paste(value, collapse="\n")
                             },
                             get_json_data=function(...) {
                               toJSON(get_value())
                             },
                             set_value=function(val, ...) {
                               "Set as character vector of lines"
                               tmp <- unlist(strsplit(val, "\\n"))
                               if(length(tmp) == 0)
                                 tmp <- ""
                               value <<- tmp
                               ### Old call_Ext("setValue", escapeSingleQuote(paste(value, collapse="\n")))
                               callback <- "
function(data, textStatus, jqXHR) {
  {{id}}.setValue(data[0])
}
"
                               add_async_javascript_callback("base_url + 'runProxy'",
                                                             whisker.render(callback, list(id=get_id())),
                                                             data=list(id=get_id()))
                               
                             },
                             set_editable=function(val, ...) {
                               "readonly? Then set to FALSE."
                               ..editable <<- as.logical(val)
                               cmd <- sprintf("%s.editor.setOption('readOnly', %s);",
                                              get_id(),
                                              ifelse(val, "false", "true")
                                              )
                             },
                             get_editable=function(...) {
                               ..editable
                             },
                             ## handler code
                             ## override
                             connect_to_toolkit=function(...) {},

                             ## process transport bit by bit. Alternative to doing an entire chunk
                             set_line=function(line, from, to, val) {
                               "Helper function to process values from codemirror"
                               out <- value
                               if(line > length(out))
                                 out[line] <- ""
                               l <- out[line]
                               tmp <- strsplit(l, "")[[1]]
                               if(from < to) {
                                 ## replace
                                 tmp <- tmp[-(from:to)]
                                 tmp[from] <- val[1]                                 
                               } else if(from == to) {
                                 ## insert
                                 if(from == 1)
                                   tmp <- c(val[1], tmp)
                                 else if(from >= length(tmp))
                                   tmp <- c(tmp, val[1])
                                 else
                                   tmp <- c(tmp[1:from], val[1], tmp[(from+1):length(tmp)])
                               }
                               tmp[is.na(tmp)] <- ""
                               out[line] <- paste(tmp, collapse="")
                               value <<- out

                               if(length(val) > 1)
                                insert_line(line + 1, val[-1])
                             },
                             set_multiline=function(from, to, val) {
                               from <- from + 1; to <- to + 1 # 1-based
                               l_start <- from[1]; l_end <- to[1]
                               ind_start <- from[2]; ind_end <- to[2]
                               out <- value
                               ## we need to stich up between start and end
                               tmp <- strsplit(out[l_start], "")[[1]]
                               if(ind_start == 1)
                                 tmp <- val[1]
                               else
                                 tmp <- c(tmp[1:(ind_start-1)], val[1])
                               
                               tmp_to <- strsplit(out[l_end], "")[[1]]
                               if(ind_end <= length(tmp_to))
                                 tmp_to <- tmp_to[(ind_end):length(tmp_to)]
                               else
                                 tmp_to <- character(0)
                               tmp <- c(tmp, tmp_to)
                               out[l_start] <- paste(tmp, collapse="")
                               ##
                               if(l_start + 1 <= l_end)
                                 out <- out[-((l_start+1):(l_end))]
                               value <<- out
                               if(length(val) > 1)
                                 insert_line(l_start, val[-1])
                             },
                             ## handle:
                             ## same line insert
                             ## same line delete
                             ## mutiline insert
                             ## multiline delete
                             insert_line=function(ind, val) {
                               ## 0 - begin, n end
                               if(ind <= 0) {
                                 value <<- c(val[1], value)
                               } else if(ind >= (n <- length(value))) {
                                 value[ind] <<- val[1]
                               } else {
                                 value <<- c(value[1:ind], val[1], value[(ind+1):n])
                               }
                               if(length(val) > 1)
                                 insert_line(ind + 1, val[-1])
                             },
                             process_transport=function(val, ...) {
                               "Process codemirror transport piece"
                               ## val has from, to, text and possibly next

                               if(missing(val) || is.null(val$text)) return()

                               from <- val$from; to <- val$to
                               if(from['line'] == to['line']) {
                                 line <- from['line'] + 1
                                 set_line(line, from['ch'] + 1, to['ch'] + 1,
                                          val$text)
                               } else {
                                 set_multiline(from, to, val$text)
                               }
                               ## recurse if requested
                               if(!is.null(val[['next']]))
                                 process_transport(val[['next']])
                             }


                             ))

## Modifications to use plain text editing
GCodeMirrorPlainText <- setRefClass("GCodeMirrorPlainText",
                                    contains="GCodeMirror",
                                    methods=list(
                                      init=function(
                                        text, font.attr, wrap, handler, action, container, ...,
                                        width, height, 
                                        ext.args=list()
                                        ) {

                                        ext.args <- getWithDefault(ext.args, list())
                                        ext.args <- merge(ext.args,
                                                          list(
                                                               lineNumbers=FALSE,
                                                               matchBrackets=FALSE,
                                                               mode="null",
                                                               lineWrapping=TRUE
                                                               )
                                                          )
                                        callSuper(text, container, ..., width=width, height=height, ext.args=ext.args)
                                      }
                                      ))
