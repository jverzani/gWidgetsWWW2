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


##' @include array.R
##' @include ext-misc.R
##' @include BasicInterface.R
##' @include gwidgets-toplevel.R
NULL

## Main class for object, include Ext stuff


                         
##' Base class for all Ext objects
##'
##' The \code{GComponent} class provides the base class for all widgets.
##' 
##' The basic setup of gWidgetsWWW2 involves a set of constructors and
##' S3 methods for manipulating the constructed objects. The
##' constructors return a reference class object. The S3 methods then
##' simply pass arguments along to the appropriate reference class
##' method. Of course, these may be called directly, but for
##' portability of gWidgets code to other toolkits this is not
##' recommended. However, for methods that are only implemented in a
##' given toolkit, such reference method calls becomes necessary and
##' desirable. In documenting the reference class objects just those
##' exposed methods which must be called as reference methods are
##' mentioned.
##'
##' The GComponent class is used to define methods common to  all the
##' widgets. This class also includes methods for processing
##' callbacks. This is different in gWidgetsWWW2. The basic idea is
##' that JavaScript is used to make a callback into the session
##' containing (using the  session id to find the correct evaluation
##' environment). Passed along are an object id (to find the signaling
##' object), a signal (to look up the handlers assigned to that object
##' for the given signal) and possibly some extra parameters. The
##' latter are there to bypass the transport calls that are used to
##' synchronize the widget state from the browser with the R session
##' data. These transport calls are asynchronous so may not have been
##' processed when the handler call is processed.
##'
##' The lookup used above requires each widget to be registered in a
##' toplevel object which is unique to a page. This toplevel object is
##' found from the session id, which then looks up the object from the
##' passed in object id. This toplevel object is passed into a widget
##' via either the \code{container} argument or the \code{parent}
##' argument. In addition to routing requests to handlers, the
##' toplevel object also is used to send back JavaScript commands to
##' the browser. The method \code{add_js_queue} is all that is needed
##' for this. A convenience method \code{call_Ext} provides an
##' alternative. For this method one specifies a method name and named
##' arguments that are converted to a JavaScript object to
##' parameterize the method call. Somewhat reverse to this is calling
##' an R object from JavaScript. The method \code{call_rpc} is used
##' for this, where in the JavaScript code one uses \code{jRpc} to
##' initiate the call and \code{add_public_method} to register that an
##' object's method is available to be called in this manner.
##' 
##' Methods related to the handler code are \code{add_handler},
##' \code{invoke_handler}, \code{handler_widget},
##' \code{connect_to_toolkit},  \code{transport_fun},
##' \code{process_transport}, \code{param_defn},
##' \code{before_handler}
##'
##'
##' The basic reference class interface is meant to be implemented by
##' all gWidgets implementations. The forthcoming gWidgets2 will
##' expect that. So gWidgets2WWW -- if that ever happens -- will use
##' the basic interface, but for now gWidgetsWWW2 does only for the
##' most part.
##' 
##'  @rdname gWidgetsWWW2-package
GComponent <- setRefClass("GComponent",
                         contains="BasicToolkitInterface",
                         fields=list(
                           "toplevel"="ANY",            # toplevel instance
                           "constructor" = "character", # for write_constructor
                           "args"="ANY",            # for write_constructor
                           "prefix"="character",        # really just "o"
                           "id" = "character",          # the actual id, e.g gWidget_ID2
                           "transport_signal"="character", # if given, the signal to initiate transport
                           "value"="ANY",                   # default property of the object
                           "public_methods"="character",                # exposed to rpc call
                           ##
                           ".e"="environment",          # for stashing things (tag, tag<-)
                           change_signal="character", # what signal is default change signal
                           connected_signals="list",
                           ..visible="logical",
                           ..enabled="logical",
                           ..editable="logical",
                           ..index="ANY"
                           ),
                         methods=list(
                           initialize=function(container=NULL, parent=NULL,  ...) {
                             "Initialize widget. @param toplevel required or found"
                             if(!is.null(toplevel) && is(toplevel, "GWidgetsTopLevel")) {
                               toplevel <<- toplevel
                             } else {
                               if(is.null(container) & is.null(parent))
                                 toplevel <<- NULL
                               if(is.null(container) & !is.null(parent))
                                 toplevel <<- parent$get_toplevel()
                               if(!is.null(container))
                                 toplevel <<- container$get_toplevel()
                             }

                             ## This invovles a hack for finding the
                             ## toplevel when none is specified, which
                             ## should only happen with the initial
                             ## gwindow() call.
                             ## insert a toplevel thingy
                             if(is.null(toplevel)) {
                               ## this variable is created when a new session is and lives in the
                               ## evaluation environment. See gwidgets-session
                               if(exists(".gWidgets_toplevel", inherits=TRUE))
                                  toplevel <<- get(".gWidgets_toplevel", inherits=TRUE)
                               else
                                 toplevel <<- gWidgetsWWW2:::GWidgetsTopLevel$new()
                             } 
                             ## work on id
                             ## id is used for html DOM id, o+id is used for
                             ## javascript variable and key within toplevel hash
                             ## to refer to this object

                             initFields(
                                        args=gWidgetsWWW2:::ExtArgs$new(), # append with add_args method
                                        .e=new.env(),
                                        prefix="o",
                                        id=sprintf("gWidget_ID%s", toplevel$get_object_id()),
                                        ##
                                        ..visible=TRUE,
                                        ..enabled=TRUE,
                                        ..editable=TRUE,
                                        ..index=NULL,
                                        transport_signal="",
                                        change_signal="",
                                        public_methods=character(0)
                                        )
                             
                             args$extend(list(id=id))
                             toplevel$add_object(.self, get_id())
                             
                             ## return. We've eaten all the arguments
                             ## we know of by now
                             callSuper()
                           },
                           init=function(...) {
                             "Initialization of the widget. The initialize method takes care of the toplevel"
                             
                           },
                           ## helpers
                           has_slot=function(key) {
                             exists(key, .self, inherits=FALSE)
                           },
                           ##
                           add_args = function(..., overwrite=TRUE) {
                             "add new arguments. Will overwrite. Pass in lists of arguments through ..."

                             sapply(list(...), args$extend, overwrite=overwrite)
                           },
                           ## id of base object (ogWidgetID1) There
                           ## are two ids: the object id returned here
                           ## refers to the actual javascript object
                           ## created. The other id (the id property)
                           ## is the DOM id of the object. The object
                           ## id is usually what we want.
                           get_id = function() {
                             "ID of object. There is DOM id store in id property and Ext object id returned by this"
                             sprintf("%s%s", prefix, id)
                           },
                           get_toplevel=function() {
                             if(!is.null(toplevel))
                               return(toplevel)
                             else if(is(.self, "GWidgetsTopLevel"))
                               return(.self)
                             else
                               return(NULL)
                           },
                           ## interface with js queue in toplevel
                           add_js_queue = function(cmd) {
                             "Add command to JavaScript queue"
                             toplevel$js_queue_push(cmd)
                           },
                           flush_js_queue = function() {
                             "Flush commands in JavaScript queue"
                             toplevel$js_queue_flush()
                           },

                           ## method to write out constructor
                           write_constructor = function() {
                             "Write out constructor."
                             cmd <- sprintf("var %s = new %s(%s);",
                                            get_id(),
                                            constructor,
                                            args$to_js_object()
                                            )
                             add_js_queue(cmd)
                           },
                           write_ext_object = function(cls, args) {
                             "Write out an Ext object converting args"
                             cmd <- sprintf("new %s(%s)", cls, toJSObject(args))
                             String(cmd)
                           },
                           process_dot_args = function(...) {
                             "Helper function"
                             l <- list(...)
                             out <- sapply(l, coerceToJSString)
                             paste(out, collapse=", ")
                           },
                           ## call a method of ext object
                           ## This converts its arguments to JavaScript strings through coerceToJSString
                           call_Ext = function(meth, ...) {
                             "Write JavaScript of ext method call for this object. The ... values will be coerced to JavaScript stings through coerceToJSString, allowing the call to be as 'R'-like as possible, e.g.: call_Ext('setValue', 'some value'). Here the string will be quoted through ourQuote. To avoid that wrap within the String function, as in call_Ext('setValue', String('some value'))."

                             cmd <- sprintf("%s.%s(%s);",
                                            get_id(),
                                            meth,
                                            process_dot_args(...))
                             cmd
                             add_js_queue(cmd)
                           },
                           ## Ext apply basically merges lists (objects). Where the default comes from is up to you to
                           ## read about....
                           ext_apply = function(value) {
                             "Call ext apply with value a list containing config options. This is called after write-constructor, prior to that call use add_args or arg$append."
                             if(is.null(value))
                               return()
                             cmd <- sprintf("Ext.apply(%s, %s);",
                                            get_id(),
                                            toJSObject(value))
                             add_js_queue(cmd)
                           },

                           ## Transport. Many widgets transport a value from WWW -> R after
                           ## minor changes through an AJAX call. This requires three things.
                           ## 1. a signal that is listened to for an initiation of the transport
                           ## 2. a function to define an object {value: ..., values: ..., others: ...} that
                           ##    is converted to JSON and transported back to R through the param argument
                           ## 3. a process_transport method that is passed the widget id and this param value. It
                           ##    adjusts the state of the R widget and optionally other call, returning the javascript
                           ##    queue when done
                           ##
                           ## In the case where the transport_signal is the same as the default change_signal -- where we pass in the
                           ## the change information -- we bypass this call.
                           transport_fun = function() {
                             "javascript function for transport web -> R. Creates an object param.
This is a string to be passed to the javascript queue withing the transport function call
E.g. var param = {value: this.getText()}"
                             "var param = null;"         # no default
                           },
                           write_transport = function() {
                             "Writes out JavaScript for transport function"
                             ## param ? Ext.JSON.encode(param) : null
                             f <- function(t_signal) {
                               if(t_signal == "") return()
                               cmd <- sprintf("%s.on('%s', function(%s) {%s; transportFun('%s', param)}, null, {delay:10, buffer:100, single:false});",
                                              get_id(),
                                              t_signal,
                                              getWithDefault(.ext_callback_arguments[[t_signal]], ""),
                                              transport_fun(),
                                              get_id()
                                              )
                               add_js_queue(cmd)
                             }
                                   
                             sapply(transport_signal, f)
                           },
                           write_change_transport = function() {
                             "Write change handler, instead of transport"
                             if(change_signal != "")
                               add_handler(change_signal, NULL, NULL)
                           },
                           process_transport = function(value, ...) {
                             "R Function to process the transport. Typically just sets 'value', but may do more. In the above example, where var param = {value: this.getText()} was from transport_fun we would get the text for value"

                             if(!is.null(value))
                               value <<- value
                           },
                           ## Call back code
                           ##
                           is_handler=function(handler) {
                             !missing(handler) && is.function(handler)
                           },
                           ## add a handler
                           ## creates an observer arranges to connect to toolkit
                           add_handler=function(signal, handler, action=NULL, decorator, ...) {
                             "Uses Observable framework for events. Adds observer, then call connect signal method. Override last if done elsewhere"
                             ID <- NULL
                             if(is_handler(handler)) {
                               if(!missing(decorator))
                                 handler <- decorator(handler)
                               o <- observer(.self, handler, action) # in gWidgets2 but not now
                               ID <- add_observer(o, signal)
                             }
                             connect_to_toolkit_signal(signal, ...)
                             invisible(ID)
                           },
                           invoke_handler=function(signal, ...) {
                             "Invoke observers listening to signal"
                             notify_observers(..., signal=signal)
                           },
                           handler_widget=function() {
                             "Widget to assign handler to"
                             .self
                           },
                           connect_to_toolkit_signal=function(
                             signal, # which signal
                             ...
                             ) {
                             "Connect signal of toolkit to notify observer"
                             ## only connect once
                             if(is.null(connected_signals[[signal, exact=TRUE]]))
                              add_R_callback(signal, ...)
                             connected_signals[[signal]] <<- TRUE
                           },
                           cb_args=function(signal) {
                             "Callback arguments, may be overridden in a subclass"
                             getWithDefault(.ext_callback_arguments[[signal, exact=TRUE]], "")
                           },
                           get_callback_object = function() {
                             "Return object for callback. Defaults to get_id(), but can be subclassed"
                             get_id()
                           },
                           add_R_callback = function(signal, ...) {
                             "Add a callback into for the Ext signal. Return callback idas a list."

                             ## XXX This needs a fixing. The callbacks are now stored in the objects and we
                             ## notify through toplevel$call_handler(id, signal, params)
                             ## The id is for lookup from toplevel, the signal to call the right observers
                             ## the params passed back to pass information prior to the call.

                             ## What to do with handlers?
                             
                             
                             ## create JS handler code
                             cmd <- sprintf("%s.on('%s', function(%s) {%s; callRhandler('%s', '%s', param)}, null, {delay:10, buffer:100, single:false});",
                                            get_id(),
                                            signal,
                                            cb_args(signal),
                                            param_defn(signal),
                                            get_callback_object(),
                                            signal
                                            )
                             add_js_queue(cmd)

                             ## what to return?
                           },
                           ## We have an issue: when a user initiates
                           ## an action, the state of the widget is
                           ## being transported via the transport
                           ## mechanism, Some times the request (which
                           ## is asynchronous) beats the transport and
                           ## the wrong value is used. This allows us
                           ## to bypass by signal. These defaults just
                           ## use the transport mechanism and are
                           ## meant to be overridden.
                           param_defn=function(signal) {
                             "Define different parameter definitions based on the signal"
                             if(signal == change_signal) {
                               transport_fun()
                             } else {
                               "var param = null"
                             }
                           },
                           before_handler=function(signal, params) {
                             "Hook that can be called prior to observer call. Might be useful to set value without relying on transport call to arrive first. Return value -- a named list -- is passed to observers as components of h "
                             if(signal == change_signal) {
                               process_transport(params)
                             }
                           },
                           ## delayed call
                           add_async_javascript_callback = function(url, callback,  data=list(), data_type=c("json","xml", "html", "script", "text")) {
                             "
##' add ajax call complete with handler to call
##'
##' @param url url to call. Quote it if it is a string
##' @param callback string containing javascript callback. Might be
##' callRhandler to work with gWidgets. Arguments are data, textStatus
##' and jqXHR.
##' @param data named list of values to pass back to ajax call
##' @param data_type type of data coming back
"
                             tpl <- "
$.ajax({{url}}, {
       dataType: '{{data_type}}',
       data: {{data}},
       type:'GET',
       cache:false,
       success: {{callback}}
});
"
                             out <- whisker.render(tpl, list(url=url,
                                                             data_type=match.arg(data_type),
                                                             data=toJSObject(merge.list(
                                                               list(session_id=String("session_id")),
                                                               data)),
                                                             callback=callback))
                             add_js_queue(out)
                           },
                           ## block and unblock
                           block_handlers=function() {
                             "Block all handlers."
                             ## default is to block the observers. 
                             block_observers()
                           },
                           block_handler=function(ID) {
                             "Block a handler by ID"
                             block_observer(ID)
                           },
                           unblock_handlers=function() {
                             "unblock blocked observer. May need to be called more than once to clear block"
                             unblock_observers()
                           },
                           unblock_handler=function(ID) {
                             "unblock a handler by ID"
                             unblock_observer(ID)
                           },
                           remove_handlers=function() {
                             "Remove all observers"
                             remove_observers()
                           }, 
                           remove_handler=function(ID) {
                             "remove a handler by ID"
                             remove_observer(ID)
                           },
                           ## Used to add a javascript callback -- that is not a call into R. 
                           add_js_callback = function(signal, callback, ...) {
                             "Add a javascript callback. The value of 'this' refers to the object this is called from"
                             cmd <- sprintf("%s.on('%s', %s);",
                                            get_id(),
                                            signal,
                                            callback)
                             add_js_queue(cmd)
                           },
                           ##
                           ## basic callbacks? (ext-component.R) XXX
                           ##
                           add_handler_changed=function(handler, action=NULL, ...) {
                             add_handler(change_signal, handler, action, ...)
                           },
                           invoke_change_handler=function(...) {
                             "Generic change handler invoker."
                             if(!is(change_signal, "uninitializedField") && length(change_signal))
                               invoke_handler(signal=change_signal, ...)
                           },
                           add_handler_change=function(handler, action=NULL, ...) {
                             add_handler("change", handler, action, ...)
                           },
                           add_handler_clicked=function(handler, action=NULL, ...) {
                             add_handler("clicked", handler, action, ...)
                           },
                           add_handler_focus=function(handler, action=NULL, ...) {
                             add_handler("focus", handler, action, ...)
                           },
                           add_handler_blur=function(handler, action=NULL, ...) {
                             add_handler("blur", handler, action, ...)
                           },
                           add_handler_double_click=function(handler, action=NULL, ...) {
                             add_handler("dblclick", handler, action, ...)
                           },
                           ##
                           ## rpc
                           ##
                           call_rpc = function(meth, val) {
                             "The jRpc call back from JavaScript into R passes a method name and arguments to the object. This calls the method"  
                             if(!is.list(val))
                               val <- list(val)
                             ## awkward way to call method by name avoiding cache
                             if(exists(meth, .self, inherits=FALSE))
                               f <- get(meth, .self)
                             else
                               f <- methods:::envRefInferField(.self, meth, getClass(class(.self)), .self)
                             
                             f(val)
                           },
                           add_public_method=function(x) {
                             "Add a method name to the public methods, so that jRpc can call back intoR."
                             public_methods <<- c(public_methods, x)
                           },
                           ##
                           ## Drag and Drop
                           ##
                           ## XXX implement me
                           ##
                           ## setup
                           ##
                           setup = function(container, handler, action=NULL, ext.args=NULL, ...) {
                             "Set up widget"

                             if(!is.null(ext.args))
                               args$extend(ext.args)

                             if(missing(container) || is.null(container)) {
                               message(gettext("No empty containers are allowed"))
                               stop()
                             }
                             container$add_dots(.self, ...)
                             write_constructor()
                             container$add(.self, ...)

                             
                             ## if transport & change are identical, we cut down
                             ##  by one with this.
                             if(length(transport_signal) == 1 &&
                                transport_signal == change_signal)
                               write_change_transport()
                             else
                               write_transport()

                             if(!missing(handler)  & !is.null(handler))
                               add_handler_changed(handler, action)
                           },
                           ##
                           ## Cookies
                           ##
                           ## Example:
                           ## w <- gwindow("Cookies")
                           ## g <- ggroup(cont=w)
                           ## w$set_cookie("one", "two")
                           ## b <- gbutton("click me", cont=g, handler=function(h,...) {
                           ##   print(b$get_cookies()) ## cookies at time of page initiation, so won't include "one" first time
                           ## })
                           set_cookie = function(key, value) {
                             "Set a cookie"
                             toplevel$cookies[[key]] <<- value
                           },
                           get_cookies = function() {
                             "Return list of cookies"
                             toplevel$the_request$cookies()
                           },
                           ##
                           ## The standard gWidgets API for reference methods expects certain reference methods to be defined.
                           ## see BasicInterface for these. That just presents the names, not their implementations. These are
                           ## done here.
                           ##
                           ## Basic methods for gWidgets
                           get_length=function() 1,
                           len=function() 1,
                           ## get/set value
                           coerce_to = function(val, ...) {
                             "if coerce_with property present, call function on value"
                             if(is(coerce_with, "uninitializedField") || is.null(coerce_with))
                               return(val)
                             coerce_with(val)
                           },
                           get_value = function(...) {
                             "Get main property, Can't query widget, so we store here"
                             coerce_to(value)
                           },
                           set_value=function(value, ...) {
                             "Set main property, invoke change handler on change"
                             old_value <- value
                             value <<- value
                             if(!identical(old_value, value))
                               invoke_change_handler()
                           },
                           get_index=function(drop=TRUE, ...) {
                             ..index
                           },
                           set_index=function(value, ...) {
                             old_index <- ..index
                             ..index <<- value
                             if(!identical(old_index, ..index))
                               invoke_change_handler()
                           },
                           get_visible = function() ..visible,
                           set_visible = function(value) {
                             ..visible <<- as.logical(value)
                             call_Ext("setVisible", as.logical(value))
                           },
                           get_enabled=function() {
                             ..enabled
                           },
                           set_enabled = function(value) {
                             "Disable/enable component"
                             ..enabled <<- value
                             if(value)
                               call_Ext("enable")
                             else
                               call_Ext("disable")
                           },
                           set_tooltip = function(tip) {
                             "Set tooltip for widget"
                             call_Ext("setTooltip", tip)
                           },
                           set_focus = function(value) {
                             "focus component"
                             if(value)
                               call_Ext("focus")
                           },
                           ## tag, tag<-
                           get_attr = function(key) {
                             "Persistent attribute. If key missing return names, else return value for key"
                             if(missing(key))
                               ls(.e)
                             else
                               attr(.e, key)
                           },
                           set_attr = function(key, value) {
                             "Set persistent attribute"
                             attr(.e, key) <<- value
                           },
                            set_height = function(px) {
                              "Set height in pixels"
                              call_Ext("setHeight", px)
                            },
                            set_width = function(px) {
                              "Set width in pixels"
                              call_Ext("setWidth", px)
                            },
                           set_size = function(val) {
                             "Set size, specified as width or c(width, height) or list(width,height)"
                             ## may use:
                             ## A Number specifying the new width in the Element's Ext.Element.defaultUnits (by default, pixels).
                             ## A String used to set the CSS width style.
                             if(!is.list(val)) {
                               nms <- c("width", "height")[seq_along(val)]
                               val <- setNames(as.list(val), nms)
                             }
                             width <- val$width; height <- val$height
                             
                             if(is.null(width) && is.null(height))
                               return()
                             else if(is.null(height))
                               set_width(width)
                             else if(is.null(width))
                               set_height(height)
                             else 
                               ## depends on class
                               if(is.numeric(width)) {
                                 call_Ext("setSize", width, height)
                               } else {
                                 ## This allows set_size("100%", "100%")
                                 cmd <- sprintf("%s.setSize({width:'%s',height:'%s'});",
                                                get_id(), width, height)
                                 add_js_queue(cmd)
                               }
                           },
                           get_font = function(...) {
                             message("Fonts not implemented")
                           },
                           set_font = function(value, ...) {
                             message("Fonts are not implemented. Perhaps using HTML will work.")
                           },
                           destroy = function() {
                             "destroy component"
                             call_Ext("destroy")
                           },
                           update_widget=function(...) {
                             "Update GUI, in this case recompute layout"
                             call_Ext("doLayout")
                           }
                           
                           ))


