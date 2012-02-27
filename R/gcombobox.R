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
##'
##' The default change handler differs depending whether the field is
##' editable. If not, then the handler is called before the selection
##' is finalized. Otherwise, the handler is called on the "change"
##' event, which is to track "user-initiated change is detected in the
##' value of the field." This does not always fire on selection by an
##' item. To force that, use the \code{add_handler_select} method.
##' @param items a vector of items to choose from. Coerced to
##' character class. Use \code{coerce.with} to get a numeric value,
##' say. (Not supported, but should also allow: Or a data frame with 1 column
##' (items), two columns (items, icons), or three columns (items,
##' icons, tooltip))
##' @param selected initially selected item, by index. Use \code{0L} for none.
##' @param editable logical. Does combobox allow editing. A bug (or
##' package writer's limiations) in extjs do not allow one to set the
##' value if it is a potential index (when \code{items} is
##' integer). Go figure. Use 4.0, not 5 if you want numeric values ...
##' @param coerce.with Function. If given, called on value before returning
##' @inheritParams gwidget
##' @param autocomplete If \code{TRUE}, will hide the trigger and make
##' editable. When the user types the matching values are presented.
##' @param initial.msg If \code{selected=0}, then one can assign an initial message here
##' @param tpl a template for the item (Not working! Dang....)
##' @return an ExtWidget instance
##' @note The \code{tpl} argument is not working as we'd like.
##' @export
##' @examples
##' w <- gwindow()
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
##' g <- ggroup(cont=w, horizontal=FALSE)
##' cb <- gcombobox(state.name, cont=g)
##' addHandlerChanged(cb, handler=function(h,...) {
##' galert(paste("You selected ", svalue(h$obj), sep=""), parent=w)
##' })
##' ## initial message
##' gcombobox(state.name, cont=g, selected=0, initial.msg="Choose a state...")
##' ## autocomplete
##'  cb <- gcombobox(state.name, cont=g, autocomplete=TRUE)
gcombobox <- function(items, selected=1, editable=FALSE, coerce.with=NULL,
           handler = NULL, action = NULL, container=NULL,...,
                      width=NULL,
                      height=NULL,
                      ext.args=NULL,
                      autocomplete=FALSE,
                      initial.msg="",
                      tpl=NULL
                      ) {

  cb <- GCombobox$new(container$toplevel)
  cb$init(items, selected, editable, coerce.with, handler, action, container, ...,
                width=width,
                height=height,
                ext.args=ext.args,
          autocomplete=autocomplete,
          initial.msg=initial.msg,
                tpl=tpl)
  return(cb)
}

##' Alternative name for gcomobox
##'
##' The name gdroplist is an alternative (not encouraged) for gcomobbox
##' @rdname gcombobox
gdroplist <- gcombobox

## Not working: templates; tooltips, icons; 

##Base class for gcombobox
GCombobox <- setRefClass("GCombobox",
                         contains="GWidget",
                         fields=list(
                           store="GWidgetArrayStore",
                           editable="logical"
                           ) ,
                         methods=list(
                           init=function(items, selected=1, editable=FALSE, coerce.with=NULL,
                             handler = NULL, action = NULL, container=NULL,...,
                             width=NULL, height=NULL, ext.args=NULL, autocomplete=NULL, initial.msg="", tpl=NULL) {

                             
                             editable <<- editable
                             coerce_with <<- coerce.with

                             if(is.null(tpl)) 
                               items <- .normalize(items) # give standard names


                             store <<- GWidgetArrayStore$new(container, object_data=TRUE)
                             store$init(items)
                             
                             initFields(
                                        constructor="Ext.form.field.ComboBox",
                                        change_signal=ifelse(editable, "change", "beforeselect"),
                                        transport_signal=if(editable) c("change","beforeselect") else "beforeselect"
                                        )
                             
                             if(is.null(tpl))
                               tpl <- .make_tpl(items)

                             arg_list <- list(store=String(store$get_id()),
                                              queryMode="local", ## would like "remote" for larger stores, but whatevs for now
                                              triggerAction="all",
                                              lastQuery='',
                                              ## Want to use templates here, but can't get to work
                                              ## instead use valueField and displayField defaults
                                              #displayTpl=tpl,
                                              valueField="id",
                                              valueNotFoundText="NA",
                                              displayField="name",
                                              ##
                                              width=width,
                                              height=height,
                                              editable=editable,
                                              loadingText=gettext("Loading..."),
                                              typeAhead=TRUE
                                              )
                             if(autocomplete) {
                               arg_list[['hideTrigger']] <- TRUE
                               arg_list[['editable']] <- TRUE
                               selected <- 0
                             }
                             if(selected == 0)
                               arg_list[['emptyText']] <- initial.msg
                             
                             add_args(arg_list)

                             setup(container, handler, action, ext.args, ...)
                             
                             ## load data
                             .self$store$load_data()

                             set_index(selected)
                             ## set value -- should be set_value, but isn't dispatching to right one
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
                           set_index=function(value, ...) {
                             set_value(get_items()[value],  ...)
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
                           len=function(...) base::length(get_items()),
                           .normalize=function(items) {
                             "put on standard names to match template"
                             nms <- c("name", "icon", "tooltip")
                             if(!is.data.frame(items))
                               items <- data.frame(items, stringsAsFactors=FALSE)
                             names(items) <- nms[1:ncol(items)]
                             items[[1]] <- as.character(items[[1]])
                             items
                           },
                           ## XXX This isn't working. Fix it later
                           .make_tpl=function(items) {
                             "Make template to match standard names"
                             just_name <- "<tpl for '.'><div class='x-combo-list-item'>{id} == {name}</div></tpl>"
#                             just_name <- "<h3>{name}</h3>"
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
                             add_handler("beforeselect", handler, action)
                           },
                           add_handler_change = function(handler, action=NULL) {
                             add_handler("change", handler, action)
                           }

                           ))
