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
NA

##' Radio button group
##'
##' A basic radio button group. Specify the labels through items. The main value is the label or index.
##' @param items Vector of items to choose one from.
##' @param selected index of initially selected item
##' @param horizontal logical. Horizontal or vertical layout. (See also columns)
##' @inheritParams gwidget
##' @param columns Can be used to override horizontal TRUE or FALSE
##' @return a \code{GRadio} reference class object
##' @export
##' @note the \code{[<-} method (to change the labels) is not implemented.
##' @examples
##' w <- gwindow()
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
##' rb <- gradio(state.name[1:4], cont=w)
##' addHandlerChanged(rb, handler=function(h,...) {
##'   galert(paste("You clicked", svalue(h$obj)), parent=w)
##' })
##' svalue(rb, index=TRUE) <- 1 # by index
##' svalue(rb) <- state.name[2] # by value
##' ## rb[] <- state.name[1:6] ## doesn't work!
gradio <- function(items,
                   selected = 1, horizontal=FALSE,
                   handler = NULL, action = NULL, container = NULL, ...,
                   width=NULL, height=NULL, ext.args=NULL, columns=ifelse(horizontal,length(items), 1)) {

  rb <- GRadio$new(container, ...)
  rb$init(items,
             selected, horizontal,
             handler, action, container, ...,
             width=width, height=height, ext.args=ext.args, columns=columns)
}

## Base class for gradio
## @note TODO No way to change number of radio buttons via [<- or get/set_items, as of now
GRadio <- setRefClass("GRadio",
                       contains="GWidget",
                       fields=list(
                         items="ANY",
                         horizontal="logical"
                         ),
                       methods=list(
                         init = function(items,
                           selected = 1, horizontal=FALSE,
                           handler = NULL, action = NULL, container = NULL, ...,
                           width=NULL, height=NULL, ext.args=NULL, columns=NULL) {
                           
                           items <<- items
                           horizontal <<- horizontal
                           
                           constructor <<- "Ext.form.RadioGroup"
                           transport_signal <<- "change"
                           change_signal <<- "change"

                           arg_list <- list(items=String(items_as_array()),
                                            width = width,
                                            height = height,
                                            vertical=!horizontal,
                                            columns=columns,
                                            fieldLabel=list(...)$label
                                            )
                           
                           add_args(arg_list, ext.args)

                           setup(container, handler, action, ext.args, ...)
                           
                           set_index(selected)
                           .self
                         },
                         ## main property. We store the index in value, not the label
                         get_value = function(index=FALSE, ...) {
                           "Return index"
                           items[value]
                         },
                         get_index=function(...) {
                           value
                         },
                         set_value = function(value, index=FALSE, ...) {
                           "Set value. Default is by value, not index"
                           set_index(match(value, items))
                         },
                         set_index=function(value, ...) {
                           val <- as.integer(value)
                           ## bit awkward to set the value
                           if(is.na(val)) {
                             warning(gettext("Can not set value"))
                             return()
                           }

                           ## bounds check
                           if(val < 1 || val > length(items)) {
                             warning(gettext("Trying to set value outside of range is not supported. Setting to initial button"))
                             val <- 1L
                           }

                           value <<- val
                           cmd <- sprintf("%s.items.get(%s).setValue(true)",
                                          get_id(),
                                          .self$value - 1) # 0-based arrays
                           add_js_queue(cmd)
                         },
                         get_items = function(i, ...) {
                           items[i]
                         },
                         set_items = function(items, ...) {
                           "Set items, update GUI"
                           warning(gettext("Unable to update radio button items via [<-"))
                           return()
                           ## items <<- items
                           ## XXX update radio buttons??? TODO
                         },
                         len=function(...) base::length(items),
                         ## transport, brings back index as string
                         transport_fun = function() {
                           "param={value:newValue.valueOf()};"
                         },
                         process_transport = function(value) {
                           ind <- as.numeric(value)
                           value <<- ind
                         },
                         ## helper function
                       
                           
                         items_as_array = function() {
                           "Return items as array"
                           makeRadio <- function(label, i,  name) {
                             ## inputValue is 1-based index
                             sprintf("new Ext.form.Radio({boxLabel:'%s', inputValue: %s, name:'%s'})", label, i, name)
                           }
                           makeRadio <- function(label, i, name) {
                             sprintf("{boxLabel:'%s', inputValue: %s, name:'%s', flex:1}",
                                     label, i, name)
                           }
                           buttons <- mapply(makeRadio, items, seq_along(items), rep(.self$get_id(), len=length(items)))
                           
                           out <- paste("[",
                                        paste(buttons, collapse=","),
                                        "]", sep="")
                           return(out)
                           
                         },
                         ##
                         add_handler_clicked = function(...) {
                           "Click here is change -- perhaps through some method call, not just a moust event"
                           add_handler_changed(...)
                         }
                         )
                       )
                       
                         
