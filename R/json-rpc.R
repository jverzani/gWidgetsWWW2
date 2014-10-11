##      Copyright (C) 2011, 2012  John Verzani
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


##' A Rook app to be a JSON RPC server
##'
##' This app allows the browser to call back into R by specifying an
##' object name, its method name and some parameters. The object then
##' returns JSON to the browser. Only instances of subclasses of
##' \code{JsonRPCObject} are allowed and for them only exported
##' methods may be accessed.
##'
##' Unlike other \code{gWidgetsWWW2} AJAX calls, the environment that
##' these objects lives in is typically the global environment, rather
##' than an environment that is created when a gWidgetsWWW2 page
##' is. This allows one to share such objects across many pages. In
##' the examples there is a page counter and a data set provider.
##'
##' This is not subclassed, but rather the function call
##' \code{\link{json_rpc_server}} should be made.
##' @exportClass JsonRPCServer
##' @name JsonRPCServer-class
JsonRPCServer <- setRefClass("JsonRPCServer",
                       contains="Middleware",
                       fields=list(
                         exported="character",
                         return_type="character",
                         envir="environment"
                         ),
                       methods=list(
                         initialize=function(
                           return_json=TRUE,
                           envir=.GlobalEnv,
                           ...) {

                           initFields(
                                      return_type=ifelse(return_json, "json", "javascript"),
                                      envir=envir
                                      )

                           callSuper(...)

                         },
                         call = function(env) {
                           req <- Request$new(env)

                           if(!req$post()) {
                             stop(gettext("Call with a POST request"))
                           }

                           l <- read_rook_input(req)

                           out <- lapply(l, function(item) {
                             if(!is.list(item)) item <- as.list(item)
                             if(!exists(item$obj, envir) ||
                                !is(obj <- get(item$obj, envir), "JsonRPCObject"))
                               stop("Error: no such object")

                             obj$call_method(item$meth, item$params)
                           })
                           ## what to return? If length(out) > 1, wrap in an array
                           if(length(out) > 1)
                             out <- sprintf("[%s]", paste(out, collapse=","))
                           else
                             out <- out[[1]]
                           
                           res <- Response$new(status=200L,
                                               headers=list('Content-Type'=sprintf("application/%s", return_type)),
                                               body=out
                                               )
                           res$write("")
                           res$finish()
                         },
                         read_rook_input = function(req) {
                           "Read rook.input, then convert from JSON"
                           ## Ext.Ajax.request passes in values oddly to rook. Here we take advantage of how.
                           ## XXX This needs tweaking with rapache usage
                           input <- req$env[['rook.input']]$read()
                           l <- fromJSON(rawToChar(input))
                           if(!is.list(l))
                             l <- sapply(l, identity, simplify=FALSE)
                           l
                         },
                         load=function(url="JSON_RPC", port=9000) {
                           "Load the server as Rook app."
                           R <- Rhttpd$new()
                           try(R$start(port=port), silent=TRUE)
                           R$add(RhttpdApp$new(.self, name=url))
                         }
                         ))


##' Start a json RPC server
##'
##' This JSON RPC server allows the JavaScript programmer to call back
##' into R, specifying an object by name (whose class is a subclass of
##' JsonRPCObject), a method name (in the exported methods of the
##' object) and a JSON encoded set of parameters. The server will
##' return the results of the method call as a JSON encoded
##' object. The example uses  Ext.Ajax.request to make the request and
##' Ext.json.decode to decode the response. To use this successfully,
##' one needs to write a callback in JavaScript to do something with
##' the returned value.
##'
##' The basic idea is one starts the server, then creates subclasses
##' of the JsonRPCObject class. See the example for how this can be done.
##'
##' We memoise this call, so that the server is only started one time,
##' though one could bypass this with the \code{load} reference method
##' (say one wanted multiple urls for the server).
##' 
##' @param url URL to run server under. Defaults to "JSON_RPC", so url would be (without work), "/custom/JSON_RPC"
##' @param port Which port to start Rhttpd, if Rhttpd server has not already started.
##' @param envir Where to look for JsonRPCObjects, defaults to global environment
##' @seealso \code{JsonRPCObject}
##' @export
##' @examples
##' \dontrun{
##' ## Create a PageCounter object using JSON_RPC
##' w <- gwindow("PageCounter")
##' g <- ggroup(cont=w)
##' page_ctr <- glabel("replace me below...", cont=g)
##' 
##' 
##' ## Run this once
##' ## It sets up the PageCounter JsonRPCObject and starts the server
##' where <- .GlobalEnv ## where to look for objects
##' if(!exists("PageCounter", where)) {
##'   json_rpc_server(envir=where)
##'   PageCounter <- setRefClass("PageCounter",
##'                              contains="JsonRPCObject",
##'                              fields=list(
##'                                ctr="list"
##'                                ),
##'                              methods=list(
##'                                initialize=function(...) {
##'                                  ctr <<- list()
##'                                  to_export <- c("value") # exported methods
##'                                  callSuper(to_export, ...)
##'                                },
##'                                get_count = function(page, ...) {
##'                                  ## could do something with a file here
##'                                  ## should do file locking though!
##'                                  ## f <- "/tmp/ctr.txt"
##'                                  ## if(file.exists(f)) {
##'                                  ##   ctr <- read.table(f, 
##'                                  ##                     colClasses=c("character", "integer"),
##'                                  ##                     header=TRUE)
##'                                  ## } else {
##'                                  ##   ctr <- data.frame(page=character(0), count=integer(0), 
##'                                  ##                     stringsAsFactors=FALSE)
##'                                  ## }
##'                                  ## if(!is.na(ind <- match(page,ctr[,1]))) {
##'                                  ##   count <- ctr[ind,2] + 1
##'                                  ##   ctr[ind,2] <- count
##'                                  ## } else {
##'                                  ##   count <- 1
##'                                  ##   ctr[nrow(ctr) + 1, ] <- list(page, count)
##'                                  ## }
##'                                  ## write.table(ctr, file=f, row.names=FALSE)
##'                                  return(count)
##' 
##'                                  
##'                                  if(is.null(ctr[[page]]))
##'                                    count <- 1
##'                                  else
##'                                    count <- ctr[[page]] + 1
##'                                  ctr[[page]] <<- count
##'                                  return(count)
##'                                },
##'                                value=function(page, ...) {
##'                                  val <- sprintf("Page accessed %s times", get_count(page))
##'                                  print(val)
##'                                  toJSON(list(text=val))
##'                                }
##'                              ))$new()
##'   ## JsonRPCObjects are found in the global environment by default
##'   assign("PageCounter", PageCounter, where)
##' }
##' 
##' ## Now add json_rpc call to javascript queue setText method for a
##' ## label widget. See the set_value reference method to know. The JavaScript function
##' ## json_rpc is defined in the \code{load_AJAX.rhtml} file.
##' tpl <- "
##' json_rpc('PageCounter', 'value',{page: '{{app_name}}' },
##' function(response) {
##'   txt = Ext.JSON.decode(response.responseText);
##'   {{id}}.setText(txt.text);
##' });
##' "
##' cmd <- whisker.render(tpl, list(id=page_ctr$get_id(),
##'                                 app_name="page_counter_example"))
##' 
##' ## Add the JavaScript command to the queue so that the browser will
##' ## get it when the queue is flushed
##' w$add_js_queue(cmd)
##' }   
json_rpc_server <- function(url="JSON_RPC", port=9000, envir=.GlobalEnv) {
  ..json_rpc_server(url=url, port=port, envir=envir)
}
## only want to call the above once so we memoize it. The above call is just so roxygen
## can find the arguments
.json_rpc_server <- function(url="JSON_RPC", port=9000, envir=.GlobalEnv) {
  JsonRPCServer$new(envir=envir)$load(url=url, port=port)
}
..json_rpc_server <- memoise(.json_rpc_server)


##' Base class to subclass to make a JsonRPCObject.
##'
##' A JsonRPCObject can expose methods that may be called by name by a
##' client. See the function \code{json_rpc} for such a call. There
##' needs to be the object name, the method name, any parameters
##' (passed as a JavaScript object) and a callback which is run when
##' the result is a success. See \code{\link{json_rpc_server}} for an
##' example.
##'
##' This is intended to be subclassed.
##' @examples
##' ## a subclass to return a data set
##'  DataSets <- setRefClass("DataSets",
##'                           contains="JsonRPCObject",
##'                           fields=list(
##'                             ctr="list"
##'                             ),
##'                           methods=list(
##'                             initialize=function(...) {
##'                               ctr <<- list()
##'                               to_export <- c("get_data") # exported methods
##'                               callSuper(to_export, ...)
##'                             },
##'                             get_data = function(name, column_mapping=list(), ...) {
##'                               "return data frame, possibly changing column names"
##'                               x <- get(name)
##'                               if(!is(x, "data.frame"))
##'                                 stop("trying to access a non-data frame")
##' 
##'                               nms <- names(x)
##'                               for(i in names(column_mapping)) {
##'                                 if(!is.na(ind <- match(i, nms)))
##'                                   names(x)[ind] <- column_mapping[[i]]
##'                               }
##'                               toJSON(list(data=x))
##'                             }
##'                             ))$new()
##' ## Call with something like this:
##' w <- gwindow("JSON-RPC test")
##' ## Now add json_rpc call to javascript queue
##' cmd <- "
##' json_rpc('DataSets', 'get_data',{name: 'morley', column_mapping:{'Expt':'Experiment'}},
##' function(response) {
##'   value = Ext.JSON.decode(response.responseText);
##'   alert('There are ' + value.data.Run.length + ' observations.'); 
##' });
##' "
##' ## Add the JavaScript command to the queue so that the browser will
##' ## get it when the queue is flushed
##' w$add_js_queue(cmd)
##' @exportClass JsonRPCObject
##' @rdname JsonRPCObject
##' @name JsonRPCObject-class
JsonRPCObject <- setRefClass("JsonRPCObject",
                             fields=list(
                               exported="character"
                               ),
                             methods=list(
                               initialize=function(methods=character(0),
                                 ...) {

                                 if(length(methods))
                                   exported <<- methods

                                 callSuper(...)
                               },
                               export_method=function(meth) {
                                 exported <<- c(exported, meth)
                               },
                               call_method=function(meth, lst) {
                                 ## XXX This needs to have some process locking
                                 ## implemented!!!
                                 lst <- as.list(lst)
                                 
                                 if(meth %in% exported) {
                                   if(exists(meth, .self, inherits=FALSE))
                                     f <- get(meth, .self)
                                   else
                                     f <- methods:::envRefInferField(.self, meth, getClass(class(.self)), .self)

                                   out <- do.call(f, lst)

                                   ## should convert out to JSON!
                                 } else {
                                   out <- ""
                                 }
                                 out
                               }
                               ))

  
