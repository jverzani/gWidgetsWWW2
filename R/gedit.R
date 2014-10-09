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


##' gedit widget
##'
##' @param text initial text
##' @param width width in characters. Converted to pixels by multiplying by 8.
##' @param coerce.with Function to call for coercion from text. If no
##' coercion be careful when using the values, as the user can
##' potentialy type in malicious things.
##' @param initial.msg initial message to user, in greyed out text
##' @param validate.type pre-defined validation type:
##' \code{c("alpha","alphanum","numeric",
##' "email","url","nonempty")}. When specified, value is verified to
##' match this type.
##' @param validate.regexp optional regular expression to validate
##' against. If the argument has a names argument, this is used to
##' notify the user why the type is invalid.
##' @inheritParams gwidget
##' @return a \code{GEdit} referece class object
##' @export
##' @note The 'change' event varies from browser to browser. To catch
##' just an enter key, the \code{add_handler_enter} reference class method
##' is available.  There is no [<- method to add typeahead
##' (autocomplete) values. This feature can be done with a combobox though.
##' @examples
##' w <- gwindow()
##' sb <- gstatusbar("Powered by gWidgetsWWW2 and Rook", cont=w)
##' g <- ggroup(cont=w, horizontal=FALSE)
##' e <- gedit("initial text", cont=g)
##' addHandlerChanged(e, handler=function(h,...) {
##' galert(paste("You entered", svalue(h$obj)), parent=w)
##' })
##' ## initial message
##' e1 <- gedit(initial.msg="type a value here ...", cont=g)
##' ## validation
##' e2 <- gedit(cont=g, validate.regexp=setNames("^[a-z]*$", "Lower case only"))
gedit <- function (text = "", width = 25, coerce.with = NULL, initial.msg="",
                   handler = NULL,  action = NULL, container = NULL, ...,
                   ext.args=NULL,
                   validate.type=NULL,
                   validate.regexp=NULL
                   ) {

  e <- GEdit$new(container, ...)
  e$init(text, width, coerce.with, initial.msg, handler, action, container, ...,
         ext.args=ext.args,
         validate.type=validate.type,
         validate.regexp=validate.regexp
         )
  return(e)
}



##' \code{GEdit} is base class for gedit
##' 
##' For the \code{GEdit} class, the change signal is  "blur" or a
##' focus-out event. Use the reference class method
##' \code{add_handler_enter} for "enter" key press and
##' \code{add_handler_change} for a mix of change events (browser
##' dependent, but for most includes \code{['change', 'input',
##' 'textInput', 'keyup', 'dragdrop']}.
##' @rdname gedit
GEdit <- setRefClass("GEdit",
                     contains="GWidgetText",
                     fields=list(
                       stub="ANY"
                       ),
                     methods=list(
                       init=function(text = "", width = 25, coerce.with = NULL, initial.msg="",
                         handler = NULL,  action = NULL, container = NULL, ...,
                         ext.args=NULL,
                         validate.type=NULL,
                         validate.regexp=NULL
                         ) {

                         if(!is.null(coerce.with) && is.character(coerce.with))
                           coerce.with <- get(coerce.with, inherits=TRUE)
                         
                         initFields(
                                    value=text,
                                    coerce_with=coerce.with,
                                    constructor="Ext.form.field.Text",
                                    transport_signal=ifelse(is_running_local(), "change","blur")
                                    ### transport_signal="change",
                                    ###change_signal="blur"
                                    )
                         ## would like to slow down number of transport calls. Some cobo of blur and <enter>?
                         ##transport_signal=ifelse(is_running_local(), "keyup", "blur"),
                         
                         ## constructor arguments
                         arg_list <- list(value = text,
                                          enableKeyEvents=TRUE,
                                          width = 8 * width,
                                          fieldLabel=list(...)$label
                                          )
                         ## empty text
                         if(nchar(text) == 0)
                           arg_list[['emptyText']] <- initial.msg
                         
                         ## Validation
                         if(!is.null(validate.type)) {
                           if(validate.type == "nonempty") {
                             arg_list[['allowBlank']] <- FALSE
                           } else if(validate.type == "numeric") {
                             ## need to supply a regular expression
                             validate.regexp <- setNames("[0-9\\.e]", gettext("Not a number"))
                             arg_list[['allowBlank']] <- FALSE
                           } else {
                             arg_list[['vtype']] <- validate.type
                             arg_list[['validateOnChange']] <- TRUE
                           }
                         }
                         if(!is.null(validate.regexp)) {
                           tpl <- "
function(value) {
  var regex = new RegExp('{{regex}}');
  return (regex.test(value)) ? true : '{{reason}}'
}
"
                           
                           arg_list[['validator']] <- String_render(tpl,
                                                                    list(regex=validate.regexp,
                                                                         reason=ifelse(!is.null(tmp <- names(validate.regexp)), tmp[1], gettext("does not match desired value"))
                                                                         ))
                           arg_list[['validateOnChange']] <- TRUE
                         }
                       
                         
                         add_args(arg_list)

                         setup(container, handler, action, ext.args, ...)

                         ## transport on Enter too
                         add_handler_enter(function(h,...) {
                           message("set value from enter: ", h$value)
                           svalue(h$obj) <- h$value
                         })
                         
                       },

                       transport_fun = function() {
                         "var param = {value: w.getValue()}"
                       },
                       process_transport=function(value, ...) {
                           ## XXX should decode url encode here
                           ## message("Transport process", value)
                         
#                         if(!is.null(value))
#                           value <<- utils:::URLdecode(value)
                         value <<- value
                       },

                       param_defn=function(signal) {
                         if(signal == "blur") { ##change_signal) {
                           sprintf("var param = {value: Ext.htmlEncode(%s.getValue())};", get_id())
                         } else if(signal == "keyup") {
                           "var param = {key: e.getKey()};"
                         } else if(signal == "change") {
                           "var param = {value: newValue};"
                         } else {
                           "var param = null;"
                         }
                       },
                       before_handler=function(signal, value) {
                         if(signal == "keyup") {
                           ## how to process?
                         } else if(signal == "blur") {
                           set_value(value)
                         } else if(signal == "enterkey") {
                           set_value(value)
                         }
                         
                       },
                       ## handlers
                       add_handler_changed=function(handler, action=NULL, ...) {
                         add_handler("blur",  handler, action, ...)
                       },
                       add_handler_keystroke=function(handler, action=NULL, ...) {
                         "Keystroke is only normalized charCode. Not a letter!"
                         add_handler("keyup",  handler, action, ...)
                       },
                       add_handler_enter=function(handler, action=NULL, ...) {
                         "add handler key for enter event. No addHandlerEnter method, call this directly"
                         signal <- "enterkey"
                         o <- observer(.self, handler, action)
                         add_observer(o, signal)
                         tpl <- "
{{id}}.on('specialkey', function(w, e, opts) {
  if(e.getKey() == e.ENTER) {
    callRhandler('{{id}}', '{{signal}}', {value: Ext.htmlEncode(w.getValue())});
  }
});
"
                         cmd <- whisker.render(tpl,
                                               list(id=get_id(), signal=signal))

                         add_js_queue(cmd)
                       }
                       
                       ))
                     
