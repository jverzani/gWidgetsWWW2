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

##' @include gwidget-proxy.R
##' @include icons.R
NULL

##' combobox implementation
##'
##' The \code{svalue<-} method is used to specify value by name or by
##' index. The \code{[<-} method can be used to update the data to
##' select from.
##' @param items a vector of items to choose from. (Not supported, but
##' should be: Or a data frame with 1 column (items), two columns
##' (items, icons), or three columns (items, icons, tooltip))
##' @param selected initially selected item, by index. Use \code{0L} for none.
##' @param editable logical. Does combobox allow editing. A bug (of
##' package writer's limiations) in extjs do not allow one to set the
##' value if it is a potential index. Go figure. Use 4.0, not 5 ...
##' @param coerce.with Function. If given, called on value before returning
##' @param handler handler
##' @param action action
##' @param container parent container
##' @param ... passed to \code{add} method of parent
##' @param width width
##' @param height height
##' @param ext.args extra arguments to pass to constructor
##' @param tpl a template for the item (Not working!)
##' @return an ExtWidget instance
##' @note The \code{tpl} argument is not working as we'd like.
##' @export
##' @examples
##' w <- gwindow()
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
##' cb <- gcombobox(state.name, cont=w)
##' addHandlerChanged(cb, handler=function(h,...) {
##' galert(paste("You selected ", svalue(h$obj), sep=""), parent=w)
##' })
##' ## type ahead style
##' cb$hide_trigger(TRUE)
gcombobox <- function(items, selected=1, editable=FALSE, coerce.with=NULL,
           handler = NULL, action = NULL, container=NULL,...,
                      width=NULL,
                      height=NULL,
                      ext.args=NULL,
                      tpl=NULL
                      ) {

  cb <- GCombobox$new(container$toplevel)
  cb$init(items, selected, editable, coerce.with, handler, action, container, ...,
                width=width,
                height=height,
                ext.args=ext.args,
                tpl=tpl)
  return(cb)
}

##' Alternative name for gcomobox
##'
##' The name gdroplist is an alternative (not encouraged) for gcomobbox
##' @name gcombobox
gdroplist <- gcombobox

## Not working: templates; tooltips, icons; 

##' Base class for gcombobox
##' @name gcombobox-class
GCombobox <- setRefClass("GCombobox",
                         contains="GWidget",
                         fields=list(
                           store="GWidgetArrayStore",
                           editable="logical"
                           ) ,
                         methods=list(
                           init=function(items, selected=1, editable=FALSE, coerce.with=NULL,
                             handler = NULL, action = NULL, container=NULL,...,
                             width=NULL, height=NULL, ext.args=NULL, tpl=NULL) {

                             ## We store the value not the index
                             if(as.integer(selected) > 0)
                               selected <- items[selected]
                             else
                               selected <- NA

                             editable <<- editable
                             coerce_with <<- coerce.with

                             if(is.null(tpl)) 
                               items <- .normalize(items) # give standard names


                             tpl <- "<tpl for '.'><div >{id} == {name}</div></tpl>"

                             store <<- GWidgetArrayStore$new(container)
                             store$init(items)
                             
                             initFields(
                                        constructor="Ext.form.field.ComboBox",
                                        change_signal=ifelse(editable, "change", "beforeselect"),
                                        transport_signal=ifelse(editable, "change", "beforeselect")
                                        )
                             
                             if(is.null(tpl))
                               tpl <- .make_tpl(items)

                             arg_list <- list(store=String(store$get_id()),
                                              queryMode = "remote",
                                              triggerAction="all",
                                              lastQuery='',
                                              ## Want to use templates here, but can't get to work
                                              ## instead use valueField and displayField defaults
                                              #tpl=tpl,
                                              valueField="id",
                                              displayField="name",
                                              ##
                                              width=width,
                                              height=height,
                                              editable=editable,
                                              loadingText=gettext("Loading..."),
                                              typeAhead=TRUE
                                              )
                             add_args(arg_list)

                             setup(container, handler, action, ext.args, ...)
                             
                             ## load data
                             .self$store$load_data()
                             ## set value -- should be set_value, but isn't dispatching to right one
                             set_value(selected)
                             ## if(!is.na(selected)) {
                             ##   value <<- selected
                             ##   call_Ext("setValue", selected)
                             ## } else {
                             ##   value <<- NA
                             ## }
                             
                           },
                           transport_fun = function() {
                             "param = {value:this.getRawValue()};"
                           },
                           process_transport=function(value, ...) {
                             callSuper(value)
                           },
                           param_defn=function(signal) {
                             if(signal == change_signal) {
                               transport_fun()
                             } else {
                               ""
                             }
                           },
                           prepare_for_handler=function(signal, params) {
                             if(signal == change_signal) {
                               process_transport(params)
                             }
                           },
                           get_value = function(index=FALSE, ...) {
                             "Return stored value"
                             value
                           },
                           get_index=function(...) {
                             match(value, get_items())
                           },
                           set_value = function(value, ...) {
                             "Set stored value. We store value, not index"
                             value <<- value

                             if(!is.na(value) && length(value) && nchar(value))
                               call_Ext("setValue", value)
                             else
                               call_Ext("setValue", "")
                           },
                           set_index=function(value, index, ...) {
                             set_value(get_items()[index],  ...)
                           },
                           process_transport = function(...) {
                             value <<- ..1
                           },
                           get_items = function(...) {
                             "Get items from store as vector, not data frame"
                             items <- store$get_data(...)
                             if(is.data.frame(items))
                               items[,1, drop=TRUE]
                             else
                               items
                           },
                           set_items = function(items, ...) {
                             "Set store items"
                             items <- .normalize(items)
                             store$set_data(items, ...)
                             store$load_data()
                           },
                           .normalize=function(items) {
                             "put on standard names to match template"
                             nms <- c("name", "icon", "tooltip")
                             if(!is.data.frame(items))
                               items <- data.frame(items, stringsAsFactors=FALSE)
                             names(items) <- nms[1:ncol(items)]
                             items
                           },
                           ## XXX This isn't working. Fix it later
                           .make_tpl=function(items) {
                             "Make template to match standard names"
                             just_name <- "<tpl for '.'><div class='x-combo-list-item'>{id} == {name}</div></tpl>"
                             just_name <- "<h3>{name}</h3>"
#                             paste('<tpl for="."><div class="search-item">',
#                                                '<h3><span>{name}</span></h3>',
#                                                '</div></tpl>',
#                                                sep="")
                             name_icon <- just_name
                             name_icon_tooltip <- name_icon # XXX fix
                             if(ncol(items) ==1)
                               just_name
                             else if(ncol(items) ==2)
                               name_icon
                             else
                               name_icon_tooltip
                           },
                           ## hide trigger, if want to be like gedit
                           hide_trigger = function(value) {
                             "Hide trigger if TRUE, else show"
                             call_Ext("setHideTrigger", as.logical(value))
                           },
                           ## Handlers
                           add_handler_blur = function(handler, action=NULL) {
                             add_handler("blur", handler, action)
                           },
                           add_handler_select = function(handler, action=NULL) {
                             add_handler("select", handler, action)
                           },
                           add_handler_change = function(handler, action=NULL) {
                             add_handler("change", handler, action)
                           }

                           ))
