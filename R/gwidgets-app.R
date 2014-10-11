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

appenv <- new.env()


GWidgetsAppBase <- setRefClass("GWidgetsAppBase",
                               contains="Middleware",
                               fields=list(
                                 session_manager="ANY"
                                 ),
                               methods=list(
                                 get_session = function(sessionID) {
                                   "Return session enviroment from id"
                                   e <- session_manager$get_session_by_id(sessionID)
                                   return(e)
                                 },
                                 store_session=function(id, e) {
                                   session_manager$store_session(id, e)
                                 },
                                 get_toplevel = function(sessionID, e) {
                                   "Return toplevel instance from sessionID"
                                   if(missing(e))
                                     e <- get_session(sessionID)
                                   e[[".gWidgets_toplevel"]]
                                 },
                                 ## We bypass here req$POST() as they
                                 ## have issues with characters like "&" and "+" for various
                                 ## reasons. We instead pass in JSON encoded objects through the
                                 ## the Ajax calls and read them directly here. We return a list so
                                 ## that we can use $ for extraction
                                 ##
                                 ## use read_rook_input() in place of req$GET() and req$POST()
                                 read_rook_input = function(req) {
                                   "Read rook.input, then convert from JSON"
                                   
                                   if(!is.null(req$env[['parsed.rook.input']])) {
                                     ## we have cached the input
                                     l <- req$env[['parsed.rook.input']]
                                     
                                   } else if(req$get()) {
                                     ## we just do the GET
                                     l <- req$GET()
                                     
                                   } else if(req$post()) {
                                     ## we handle post data ourselves -- if we can
                                     if(grepl("^multipart/.*boundary", appenv$.req$env$CONTENT_TYPE)) {
                                       l <- req$POST()
                                     } else {
                                       raw_input <- req$env[['rook.input']]$read()
                                       input <- rawToChar(raw_input)
                                       l <- try(fromJSON(input), silent=TRUE)
                                       if(inherits(l, "try-error")) {
                                         req$env[['rook.input']]$rewind()
                                         l <- req$POST()
                                       }
                                     }
                                   }

                                   ## store the results
                                   req$env[['parsed.rook.input']] <- as.list(l)
                                   as.list(l)
                                 }
                                 ))
                             
## This class handles the running of the script that creates the gWidgetsWWW2 app
## Before running, one can ask for a user to be authenticated.
## The main work is the method create_GUI which runs the script, which stores Javascript commands
## in the js_queue of the toplevel object, then flushes these commands when returning.
GWidgetsApp <- setRefClass("GWidgetsApp",
                           contains="GWidgetsAppBase",
                           fields=list(
                             "url"="character",
                             "gw_script"="character",
                             "app_name"="character"
                             ),
                           methods=list(
                             initialize=function(url="", app_name="", script="",
                               ##authenticator=NULL,
                               session_manager=make_session_manager(), ...) {
                               
                               initFields(
                                          url=paste("^", url, sep=""),
                                          app_name=app_name,
                                          gw_script=script,
                                          session_manager=session_manager
                                          ##
                                          )
                                          
                               
                               callSuper(...)
                             },
                             call = function(env) {
                               "Main method. Job is to create_GUI, but may need to authentiate first"

                               ## Main router function. Here we dispatch based on path_info to
                               headers <- list('Content-Type'='application/javascript')
                               status <- 200L

                               ## the appropirate method
                               req <- Request$new(env)
                               res <- Response$new(
                                                   status=status,
                                                   headers=headers
                                                   )
                               assign(".req", req, appenv) 
                               assign("app", .self, appenv)
                               
                               req_input <-  read_rook_input(req)
                               session_id <- req_input$session_id

                               out <- try(create_GUI(session_id, req), silent=TRUE)

                               if(inherits(out, "try-error")) {
                                 status <- 400L
                               } 

                               e <- get_session(session_id)
                               on.exit(store_session(session_id,e))
                               toplevel <- get_toplevel(session_id, e)
                               
                               ## write response
                               res$body <- paste(out, collapse="\n")
                               res <- Response$new(status=status,
                                                   headers=headers,
                                                   body=paste(out, collapse="\n")
                                                   )

                               e_cookies <- toplevel$cookies
#                               sapply(ls(e_cookies), function(i) res$set_cookie(i, e_cookies[[i]]))
                               toplevel$cookies <- new.env()
                               res$write("")
                               res$finish()
                             },
                            
                             script = function() {
                               "Return file name of script to process create_GUI"
                               gw_script
                             },
                            
                             create_GUI = function(session_id, req) {
                               "Run script within a new environment. Return character vector of javascript commands"

                               e <- session_manager$get_session_by_id(session_id)


                               if(is.null(e)) {
                                 e <- new.env()

                                 ## create a toplevel element and place within an evaluation environment
                                 toplevel <- GWidgetsTopLevel$new(req)
                                 toplevel$set_e(e)
                                 assign(".gWidgets_toplevel", toplevel, env=e)
                                 lockBinding(".gWidgets_toplevel", env=e)
                                 
                               } else {
                                 toplevel <- get_toplevel(e=e)
                               }

                               the_script <- script()
                               
                               if(file.exists(the_script)) {

                                 ## We use attach/detach here. This
                                 ## allows us to find the toplevel
                                 ## instance later. Likely there is a
                                 ## better way to read in the the
                                 ## script and store the results in an
                                 ## environment that can be
                                 ## serialized.
                                   attach(e, warn.conflicts=FALSE)
                                   out <- try(sys.source(the_script, envir=e, keep.source=FALSE), silent=TRUE)           #                                   detach(e)
                                   

                                 
                                 if(inherits(out, "try-error")) {
                                   stop(out)
                                 } else {
                                   cmd <- sprintf("var session_id='%s';", session_id)
                                   toplevel$js_queue$push(cmd)
                                   tpl <- "
Ext.EventManager.on(window, 'beforeunload', function(e) {close_session('{{session_id}}')}, this);
"
                                   toplevel$js_queue$push(whisker.render(tpl, list(session_id=session_id)))
                                   
                                   ## returns javascript commands
                                   x <- toplevel$js_queue$flush()
                                 }
                               } else {
                                 ## What else goes here?
                                 x <- sprintf("File '%s' does not exist", the_script)
                                 stop(x)
                               }
                               store_session(session_id, e)
                               return(x)
                             }
                             ))


##################################################

## handle AJAX calls back into the game
GWidgetsAppAjax <- setRefClass("GWidgetsAppAjax",
                            contains="GWidgetsAppBase",
                            methods=list(
                              initialize=function(session_manager=make_session_manager(), ...) {
                                initFields(
                                           session_manager=session_manager
                                          )
                               callSuper(...)
                             },
                             call = function(env) {
                               "Main method. We set up dispatch on path_info here"

                               
                               ## Main router function. Here we dispatch based on path_info to
                               ## the appropirate method
                               req <- Request$new(env)
                               assign(".req", req, appenv) 

                               e_cookies <- new.env()
                               headers <- list('Content-Type'='application/javascript')

                               ## Here we dispatch on the path_info
                               ## value. Depending on the type we
                               ## might return JSON or javascript so
                               ## we may modify the headers value that
                               ## was just set.
                               
                               if(grepl("newSessionId$", req$path_info())) {
                                 ## Create a new session and get the ID.
                                 ## Returns as JSON
                                 headers <- list('Content-Type'='application/json')
                                 out <- toJSON(list(id=session_manager$get_id()))
                               } else if(grepl("closeSession$", req$path_info())) {
                                 close_session(req)
                                 out <- ""
                               } else {
                                 ## Here we modify session environment
                                 ## we need to get then store. (Get locks, store
                                 ## unlocks)
                                 l <- read_rook_input(req)
                                 e <- get_session(l$session_id)
                                 ## close the session on exit
                                 on.exit(store_session(l$session_id, e))

                                 toplevel <- get_toplevel(e=e)

                                 if(grepl("runTransport$", req$path_info())) {
                                   ## run the transport
                                   out <- run_transport(req, toplevel)
                                 } else if(grepl("runHandler$", req$path_info())) {
                                   ## handler, return javascript queue
                                   out <- run_handler(req, e_cookies, toplevel)
                                 } else if(grepl("runProxy", req$path_info())) {
                                   ## run proxy. The proxy returns JSON code, not html
                                   if(req$post())
                                     headers <- list('Content-Type'='application/json')
                                   out <- run_proxy(req, toplevel) # run_proxy return JSON
                                 } else if(grepl("fileUploadProxy", req$path_info())) {
                                   headers <- list('Content-Type'='text/html')
                                   out <- run_upload(req, toplevel)
                                   out <- toJSON(out)
                                 } else if(grepl("runHtmlProxy$", req$path_info())) {
                                   ## run proxy that returns HTML code.
                                   headers <- list('Content-Type'='text/html')                                 
                                   out <- run_html_proxy(req, toplevel) # qurun_proxy returns HTML here, not JSON
                                 } else if(grepl("runComet$", req$path_info())) {
                                   ## handler, return javascript queue
                                   out <- run_comet(req, toplevel)
                                 } else if(grepl("runRpc$", req$path_info())) {
                                   ## do remote call return javascript queue
                                   run_rpc(req, toplevel)
                                   out <- ""
                                 }
                               }

                               
                               ## need to populate result
                               res <- Response$new(status=200L,
                                                   headers=headers,
                                                   body=paste(out, collapse="\n")
                                                   )

                               
                               ## if e_cookies do something
#                               sapply(ls(e_cookies), function(i) {res$set_cookie(i, e_cookies[[i]])})
                               
                               res$write("")
                               res$finish()
                             },
                             ## The run functions (transport, handler, proxy
                             ## These use GET, not POST, although the latter would
                             ## be appropriate for some.
                             run_transport = function(req, toplevel) {
                               "Assign values through transport. Return js commands if needed"

                               l <- read_rook_input(req)                               
                               toplevel$call_transport(l$id, l$param)
                               out <- toplevel$js_queue$flush()
                               return(out)
                             },

                             
                             run_handler = function(req, e_cookies, toplevel) {
                               "Run a handler. Return js commands if needed"

                               l <- read_rook_input(req)

                               toplevel$call_handler(l$id, l$signal, l$value, e_cookies)
                               out <- toplevel$js_queue$flush()

                               return(out)
                             },

                             run_comet = function(req, toplevel) {
                               "Run a long pull to read from queue. Returns js commands when successful"
                               l <- read_rook_input(req)
                               
                               toplevel$call_transport(l$id, l$param)
                               out <- toplevel$js_queue$flush()

                               return(out)
                             },
                              ##
                              ##
                              ##
                             run_rpc = function(req, toplevel) {
                               "Simple means to call a method fo a gWidgetsWWW2 object"

                               l <- read_rook_input(req)
                               toplevel$call_rpc(l$id, l$meth, l$value)
                               out <- toplevel$js_queue$flush()

                               return(out)
                             },
                              ##
                              ##
                              ##
                             run_proxy = function(req, toplevel) {
                               "Call proxy object to return JSON data"

                               l <- read_rook_input(req)

                               ## l has components session_id, id, params, param is json
                               id <- l$id; l$id <- NULL
                               session_id <- l$session_id; l$session_id <- NULL
                               l[['_dc']] <- NULL # ext variable


                               
                               if(req$post()){
                                 ## The editing values in gdf get passed back in a funny way
                                 ## as the use GET to pass back session_id, an did and POST to pass
                                 ## in arguments. Here we check if the toplevel found above
                                 ## is NULL, and if so we work a bit.

                                 ## do we have what we need? not if tiplevel is NULL
                                 if(is.null(toplevel)) {
                                   tmp <- req$GET()
                                   e <- get_session(tmp$session_id)
                                   toplevel <- get_toplevel(e=e)
                                   id <- tmp$id # both post and get used in this call by gdf widget
                                 }
                                 out <- toplevel$call_post_proxy(id, l) # return JSON
                               } else {
                                 out <- toplevel$call_proxy(id, l)
                               }

                               return(out)
                             },
                              run_html_proxy = function(req, toplevel) {
                                "Run the HTML proxy"
                                l <- read_rook_input(req)

                                ## l has components session_id, id, params, param is json
                                id <- l$id; l$id <- NULL
                                session_id <- l$session_id; l$session_id <- NULL
                                l[['_dc']] <- NULL # ext variable
                                out <- toplevel$call_proxy(id, l) # return HTML

                                return(out)
                              },
                             run_upload = function(req, toplevel) {
                               "Upload file"
                               l <- read_rook_input(req)
                               if(length(l) == 0)
                                 stop("No info to run file upload")
                               ## l has components session_id, id, 
                               id <- l$id; l$id <- NULL
                               session_id <- l$session_id; l$session_id <- NULL
                               l[['_dc']] <- NULL # ext variable

                               ## XXX cleam this up
                               out <- toplevel$call_upload(id, l, l) # return JSON

                               return(out)
                             },
                              ##
                              ##
                              ##
                             close_session=function(req) {
                               l <- req$GET()
                               if(length(l) == 0)
                                 stop("No info to run file upload")
                               ## l has components session_id, id,
                               session_id <- l$session_id;
                               session_manager$clear_session(session_id)
                             }
                             ))

