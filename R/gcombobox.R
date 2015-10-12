##      Copyright (C) 2011  John Verzani
##      Copyright (C) 2015  Johannes Ranke (port to R6)
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
##' index.  
##' 
##' The default change handler differs depending whether the field is
##' editable. If not, then the handler is called before the selection
##' is finalized. Otherwise, the handler is called on the "change"
##' event, which is to track "user-initiated change is detected in the
##' value of the field." This does not always fire on selection by an
##' item. To force that, use the \code{add_handler_select} method.
##' @param items a vector or data frame of items to choose
##' from. Coerced to character class. Use \code{coerce.with} to get a
##' numeric value, say. If a data frame then one can add an icon,
##' tooltip, and possibly extra information: with 1 column (items),
##' two columns (items, icons), or three columns (items, icons,
##' tooltip)). If 4 or more, then extra columns can be incorporated by
##' specifying a template. The icon is a url, not a stock name. For
##' stock icons, convert with \code{getStockIconByName} with
##' \code{css=FALSE} specified.
##' @param selected initially selected item, by index. Use \code{0L} for none.
##' @param editable logical. Does combobox allow editing. A bug (or
##' package writer's limitations) in extjs do not allow one to set the
##' value if it is a potential index (when \code{items} is
##' integer). Go figure. Use 4.0, not 5 if you want numeric values ...
##' @param coerce.with Function. If given, called on value before returning
##' @inheritParams gwidget
##' @param autocomplete If \code{TRUE}, will hide the trigger and make
##' editable. When the user types the matching values are
##' presented. (This is also the default behaviour, but if the object
##' is not editable, only valid values are stored.)
##' @param initial.msg If \code{selected=0}, then one can assign an initial message here
##' @param tpl a template for the item. This is an HTML snippet, where
##' the column names, when wrapped in braces, will be substituted for
##' the values. The first three columns have their names normalized to
##' \code{name}, \code{icon}, \code{tooltip}.
##' @return an ExtWidget instance
##' @note The \code{[<-} method is note working. The \code{tpl}
##' argument is not working as we'd like.
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
##' ## template
##' w <- gwindow()
##' g <- gvbox(container=w)
##' tmp <- "http://upload.wikimedia.org/wikipedia/commons/thumb/"
##' icons = paste(tmp, c("5/5c/Flag_of_Alabama.svg/22px-Flag_of_Alabama.svg.png",
##'                      "e/e6/Flag_of_Alaska.svg/22px-Flag_of_Alaska.svg.png",
##'                      "9/9d/Flag_of_Arizona.svg/22px-Flag_of_Arizona.svg.png"), sep="")
##' d <- data.frame(name=c("Alabama", "Alaska","Arizona"),
##'                 icon=icons,
##'                 tooltip=c("Audemus jura nostra defendere",
##'                   "North to the future",
##'                   "Ditat Deus"
##'                   ),                  
##'                 extra=c(
##'                   "Montgomery",
##'                   "Juneau",
##'                   "Phoenix"),
##'                 stringsAsFactors=FALSE)
##' cb <- gcombobox(d,
##'                 tpl="<img src=\"{icon}\"></img><span data-qtip=\"{tooltip}\"> {name},
##'                       Capital is <em>{extra}</em></span>",
##'                 cont=g)
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
GCombobox <- R6Class("GCombobox",
  inherit = GWidget,
  public = list(
    store = NULL,
    editable = NULL,
    init=function(items, selected=1, editable=FALSE, coerce.with=NULL,
                  handler = NULL, action = NULL, container=NULL,...,
                  width=NULL, height=NULL, ext.args=NULL, 
                  autocomplete=NULL, initial.msg="", tpl=NULL) {

      
      self$editable <- editable
      self$coerce_with <- coerce.with

      if(is.null(tpl)) 
        items <- self$.normalize(items) # give standard names

      self$store <- GWidgetArrayStore$new(container, object_data=TRUE)
      self$store$init(items)
      
      self$constructor <- "Ext.form.field.ComboBox"
      self$change_signal <- ifelse(editable, "change", "select")
      self$transport_signal = if(editable) c("change","select") else "select"
      
      ## cf., http://skirtlesden.com/articles/extjs-comboboxes-part-1
      if(is.null(tpl))
        tpl <- .make_tpl(items)
      tpl <- sprintf("Ext.create('Ext.XTemplate','<tpl for=\".\"><div class=\"x-boundlist-item\">%s</div></tpl>')", tpl)
      
      arg_list <- list(store=String(store$get_id()),
                       displayField="name",
                       tpl=String(tpl),
##                       triggerAction='query',
##                       queryMode='local',
                       minChars=1,
                       editable=TRUE,
                       #selectOnFocus=TRUE,
                       forceSelection=!editable,
                       growToLongestValue=TRUE,
                       typeAhead=TRUE,
                       width=width,
                       height=height,
                       fieldLabel=list(...)$label
                       )
      
      
      
      if(autocomplete) {
        arg_list[['hideTrigger']] <- TRUE
        arg_list[['editable']] <- TRUE
        selected <- 0
      }
      if(selected == 0)
        arg_list[['emptyText']] <- initial.msg
      
      self$add_args(arg_list)

      self$setup(container, handler, action, ext.args, ...)
      
      ## load data
      self$block_handlers()
      self$store$load_data(sprintf("%s.focus(true)", self$get_id()))

      self$set_index(selected)
      self$unblock_handlers()
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
#     process_transport=function(value, ...) { # JR: Commented out as another version 
#       super$process_transport(value)         #     is included below
#     },
    get_index=function(...) {
      match(value, self$get_items())
    },
    set_value = function(value, ...) {
      "Set stored value. We store value, not index"
      self$value <- value

      if(!is.na(value) && length(value) && nchar(value))
        self$call_Ext("setValue", value)
      else
        self$call_Ext("setValue", "")
    },
    set_index=function(value, ...) {
      self$set_value(get_items()[value],  ...)
    },
    process_transport = function(...) {
      self$value <- ..1
    },
    get_items = function(i,...) {
      "Get items from store as vector, not data frame"
      items <- self$store$get_data(...)
      if(is.data.frame(items))
        items[i, 1, drop=TRUE]
      else
        items
    },
    set_items = function(items, ...) {
      "Set store items"
      items <- self$.normalize(items)
      self$store$set_data(items, ...)
      self$store$load_data(sprintf("%s.focus(true)", self$get_id()))
    },
    set_focus=function(value) {
      if(as.logical(value)) {
        self$call_Ext("focus", TRUE)
      }
    },
    len=function(...) base::length(self$get_items()),
    .normalize=function(items) {
      "put on standard names to match template"
      if(!is.data.frame(items))
        items <- data.frame(items, stringsAsFactors=FALSE)
      items[[1]] <- as.character(items[[1]]) # name is character
      ## standardize first three names
      nms <- c("name", "icon", "tooltip")
      nc <- ncol(items)
      mnc <- min(3, nc)
      names(items)[1:mnc] <- nms[1:mnc]
      items
    },
    .make_tpl=function(items) {
      "Make template to match standard names: name, icon, tooltip"
      if(ncol(items) ==1)
        '{name}'
      else if(ncol(items) ==2)
        '<img src="{icon}"></img>{name}'
      else
        '<img src="{icon}"></img><span data-qtip="{tooltip}">{name}</span>'
    },
    ## hide trigger, if want to be like gedit
    hide_trigger = function(value) {
      "Hide trigger if TRUE, else show"
      self$call_Ext("setHideTrigger", as.logical(value))
    },
    ## Handlers
    add_handler_blur = function(handler, action=NULL) {
      self$add_handler("blur", handler, action)
    },
    add_handler_select = function(handler, action=NULL) {
      self$add_handler("beforeselect", handler, action)
    },
    add_handler_change = function(handler, action=NULL) {
      self$add_handler("change", handler, action)
    }

  )
)
