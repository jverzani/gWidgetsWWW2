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

##' @include ext-misc.R
##' @include utils.R
##' @include gwidgets-toplevel.R
##' @include gwidgets-session.R
NULL


##
## Base class for a gWidgetsApp.
## Used as a router for ajax requests coming back from a page
## We basically queue up javascript commands (gwidgets-toplevel)


##' Basic class for a gwidgetsWWW2 applications
GWidgetsApp <- setRefClass("GWidgetsApp",
                           contains="Middleware",
                           fields=list(
                             "url"="character",
                             "gw_script"="character",
                             session_manager="ANY"
                             ),
                           methods=list(
                             initialize=function(url="", script="",  ...) {
                               initFields(
                                          url=paste("^", url, sep=""),
                                          gw_script=script,
                                          session_manager=make_session_manager()
                                          )
                                          
                               
                               callSuper(...)
                             },
                             call = function(env) {
                               "Main method. We set up dispatch on path_info here"

                               ## Main router function. Here we dispatch based on path_info to
                               ## the appropirate method
                               req <- Request$new(env)
                               assign(".req", req, .GlobalEnv)
                               ## dispatch on path info
                               ## Might be transport -- carry value from web to R
                               ## runHandler -- run a handler, return javascripts
                               ## get session id -- called with no parameter session_id
                               ## create GUI -- called with session_id, no path_info

                               headers <- list('Content-Type'='application/javascript')

                               if(grepl("runTransport$", req$path_info())) {
                                 ## run the transport
                                 ## return javascript queue
                                 out <- run_transport(req)
                               } else if(grepl("runHandler$", req$path_info())) {
                                 ## handler, return javascript queue
                                 out <- run_handler(req)
                               } else if(grepl("runProxy", req$path_info())) {
                                 ## run proxy. The proxy returns JSON code, not html
                                 if(is.null(req$POST()))
                                    headers <- list('Content-Type'='application/json')
                                 out <- run_proxy(req) # run_proxy return JSON
                               } else if(grepl("fileUploadProxy", req$path_info())) {
                                 headers <- list('Content-Type'='text/html')                                 
                                 out <- run_upload(req)
                                 out <- toJSON(out)
                               } else if(grepl("runHtmlProxy$", req$path_info())) {
                                 ## run proxy that returns HTML code.
                                 headers <- list('Content-Type'='text/html')                                 
                                 out <- run_proxy(req) # run_proxy returns HTML here, not JSON
                               } else if(grepl("runComet$", req$path_info())) {
                                 ## handler, return javascript queue
                                 out <- run_comet(req)
                               } else if(grepl("runRpc$", req$path_info())) {
                                 ## do remote call return javascript queue
                                 run_rpc(req)
                                 out <- ""
                               } else if(grepl("newSessionId$", req$path_info())) {
                                 ## Create a new session and get the ID. Returns as JSON
                                  headers <- list('Content-Type'='application/json')
                                  out <- toJSON(list(id=session_manager$get_id()))
                                } else if(grepl("closeSession$", req$path_info())) {
                                  close_session(req)
                                  out <- ""
                                } else {
                                  ## Otherwise we create the GUI. Called as GET here
                                  session_id <- req$GET()$session_id
                                  out <- create_GUI(session_id)
                                }

                               ## need to populate result
                               res <- Response$new(status=200L,
                                                   headers=headers,
                                                   body=paste(out, collapse="\n")
                                                   )
                               res$write("")
                               res$finish()
##                               assign(".out", out, .GlobalEnv)
##                               base:::flush(stdout()) # isn't working under Linux, does this fix?
##                                flush.console() # XXX Is this an issue??? On output wasn't sent.
                             },
                             get_session = function(sessionID) {
                               "Return session enviroment from id"
                               e <- session_manager$get_session_by_id(sessionID)
                               return(e)
                             },
                             get_toplevel = function(sessionID) {
                               "Return toplevel instance from sessionID"
                               e <- get_session(sessionID)
                               e[[".gWidgets_toplevel"]]
                             },
                             script = function() {
                               "Return file name of script to process create_GUI"
                               gw_script
                             },
                             create_GUI = function(session_id) {
                               "Run script within a new environment. Return character vector of javascript commands"

                               e <- session_manager$get_session_by_id(session_id)
                               if(is.null(e)) {
                                 e <- new.env()
                                 session_manager$store_session(session_id, e)

                                 ## create a toplevel element and place within an evaluation environment
                                 toplevel <- GWidgetsTopLevel$new()
                                 toplevel$set_e(e)
                                 assign(".gWidgets_toplevel", toplevel, env=e)
                                 lockBinding(".gWidgets_toplevel", env=e)
                                 ## debug
                                 assign(".toplevel", toplevel, .GlobalEnv)
                               }
                               toplevel <- get_toplevel(session_id)

                               ## helper
                               write_error <- function(msg) {
                                 sprintf("alert('error: %s');", msg)
                               }
                               
                               the_script <- script()

                               
                               if(file.exists(the_script)) {
                                 attach(e) # attach/detach allows one to find toplevel
                                 out <- try(sys.source(the_script, envir=e), silent=TRUE)
                                 detach(e)

                                 if(inherits(out, "try-error")) {
                                   x <- write_error(out)
                                 } else {
#                                   cmd <- sprintf("var sessionID='%s';", session_id)
                                   cmd <- sprintf("var session_id='%s';", session_id)
                                   toplevel$js_queue$push(cmd)
                                   ## returns javascript commands
                                   x <- toplevel$js_queue$flush()
                                 }
                               } else {
                                 ## What else goes here?
                                 x <- write_error("File '%s' does not exist", the_script)
                               }
                               return(x)
                             },

                             ## The run functions (transport, handler, proxy
                             ## These use GET, not POST, although the latter would
                             ## be appropriate for some.
                             run_transport = function(req) {
                               "Assign values through transport. Return js commands if needed"
                               l <- req$GET()
                               if(length(l) == 0)
                                 stop("No info to run transport")
                               toplevel <- get_toplevel(l$session_id)
                               ## l has components id, param. param is json mapping to argumnets
                               toplevel$call_transport(l$id, fromJSON(l$param))
                               
                               return(toplevel$js_queue$flush())
                             },

                             
                             run_handler = function(req) {
                               "Run a handler. Return js commands if needed"
                               l <- req$GET()
                               if(length(l) == 0)
                                 stop("No info to run handler")
                               
                               ## l has components id, value. Value is json
                               toplevel <- get_toplevel(l$session_id)

                               out <- try(fromJSON(l$value), silent=TRUE)
                               if(inherits(out, "try-error"))
                                 out <- NULL
                               toplevel$call_handler(l$id, l$signal, out)
                               
                               return(toplevel$js_queue$flush())
                             },

                             run_comet = function(req) {
                               "Run a long pull to read from queue. Returns js commands when successful"
                               l <- req$GET()
                               if(length(l) == 0)
                                 stop("No info to run handler")
                               
                               ## l has components id
                               toplevel <- get_toplevel(l$session_id)
                               return(toplevel$js_queue$flush())
                             },

                             run_rpc = function(req) {
                               "Run a long pull to read from queue. Returns js commands when successful"
                               l <- req$POST()
                               if(length(l) == 0)
                                 stop("No info to run handler")
                               
                               ## l has components id
                               out <- try(fromJSON(l$value), silent=TRUE)
                               if(inherits(out, "try-error"))
                                 out <- NULL

                               toplevel <- get_toplevel(l$session_id)
                               toplevel$call_rpc(l$id, l$meth, out)

                               return(toplevel$js_queue$flush())
                             },
                             
                             run_proxy = function(req) {
                               "Call proxy object to return JSON data"
                               
                               l <- req$GET()
                               if(length(l) == 0)
                                 stop("No info to run proxy")
                               
                               ## l has components session_id, id, params, param is json
                               id <- l$id; l$id <- NULL

                               
                               session_id <- l$session_id; l$session_id <- NULL
                               l[['_dc']] <- NULL # ext variable
                               toplevel <- get_toplevel(session_id)

                               ## we may have a post request (updating a store)
                               ## or a simple GET request. This one decides
                               if(!is.null(post <- req$POST())){
                                 out <- toplevel$call_post_proxy(id, l, post) # return JSON
                               } else {
                                 out <- toplevel$call_proxy(id, l)
                               }
                               return(out)
                             },
                             run_upload = function(req) {
                               "Upload file"
                               l <- req$POST()
                               if(length(l) == 0)
                                 stop("No info to run file upload")
                               ## l has components session_id, id, 
                               id <- l$id; l$id <- NULL
                               session_id <- l$session_id; l$session_id <- NULL
                               l[['_dc']] <- NULL # ext variable
                               toplevel <- get_toplevel(session_id)
                               out <- toplevel$call_upload(id, l, req) # return JSON
                               return(out)
                             },

                             close_session=function(req) {
                               l <- req$GET()
                               if(length(l) == 0)
                                 stop("No info to run file upload")
                               ## l has components session_id, id,
                               session_id <- l$session_id;
                               session_manager$clear_session(session_id)
                             }
                             ))

