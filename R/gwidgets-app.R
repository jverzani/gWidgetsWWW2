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
##' @include Authenticator.R
NULL


##
## Base class for a gWidgetsApp.
## Used as a router for ajax requests coming back from a page
## We basically queue up javascript commands (gwidgets-toplevel)


GWidgetsAppBase <- setRefClass("GWidgetsAppBase",
                               contains="Middleware",
                               methods=list(
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
                                 ## that we can use $ for extraction
                                 read_rook_input = function(req) {
                                   "Read rook.input, then convert from JSON"
                                   req$env[['rook.input']]$rewind()
                                   input <- req$env[['rook.input']]$read()
                                   l <- fromJSON(rawToChar(input))
                                   if(!is.list(l))
                                     l <- sapply(l, identity, simplify=FALSE)
                                   l
                                 }
                                 ))
                             
##' Basic class for a gwidgetsWWW2 applications
GWidgetsApp <- setRefClass("GWidgetsApp",
                           contains="GWidgetsAppBase",
                           fields=list(
                             "url"="character",
                             "gw_script"="character",
                             "app_name"="character",
                             session_manager="ANY",
                             ##
                             authenticator="ANY",
                             ##
                             login_cookie="character",
                             login_cookie_expiry="numeric",
                             cookie_salt="character"
                             ),
                           methods=list(
                             initialize=function(url="", app_name="", script="", authenticator=NULL, ...) {
                               
                               initFields(
                                          url=paste("^", url, sep=""),
                                          app_name=app_name,
                                          gw_script=script,
                                          session_manager=make_session_manager(),
                                          ##
                                          authenticator=authenticator,
                                          login_cookie=character(0),
                                          login_cookie_expiry=1,
                                          cookie_salt="replace me"
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
                               assign(".req", req, .GlobalEnv)
                               
                               session_id <- req$GET()$session_id
                               out <- try(create_GUI(session_id, req, res), silent=TRUE)
                                 
                               if(inherits(out, "try-error")) {
                                 status <- 400L
                               } 
                               
                               ## write response
                               res$body <- paste(out, collapse="\n")
                               res <- Response$new(status=status,
                                                   headers=headers,
                                                   body=paste(out, collapse="\n")
                                                   )
                               if(length(login_cookie))
                                 res$set_cookie("gWidgetsWWW2Login", login_cookie)
                               res$write("")
                               res$finish()
                             },
                            
                             script = function() {
                               "Return file name of script to process create_GUI"
                               gw_script
                             },
                            
                             create_GUI = function(session_id, req, res) {
                               "Run script within a new environment. Return character vector of javascript commands"

                               e <- session_manager$get_session_by_id(session_id)
                               if(is.null(e)) {
                                 e <- new.env()
                                 session_manager$store_session(session_id, e)

                                 ## create a toplevel element and place within an evaluation environment
                                 toplevel <- GWidgetsTopLevel$new(req)
                                 toplevel$set_e(e)
                                 assign(".gWidgets_toplevel", toplevel, env=e)
                                 lockBinding(".gWidgets_toplevel", env=e)

                                 
                                 ## debug
                                 assign(".toplevel", toplevel, .GlobalEnv)
                               } else {
                                 toplevel <- get_toplevel(session_id)
                               }

                               ## clean up if there
                               cmd <- "
var tmp = Ext.getDom('gWidgetsLoginForm');
if(tmp) { document.body.removeChild(tmp)}
"
                                toplevel$js_queue_push(cmd)
                               
                               ## really should check if authenticator is there
                               if(!is.null(authenticator)) {
                                 out <- do_authentification(req, session_id)
                                 if(!is.null(out)) {
                                   toplevel$js_queue_push(out)
                                   return(toplevel$js_queue$flush())
                                 }
                               }

                               
                               the_script <- script()
                               
                               if(file.exists(the_script)) {
                                 attach(e) # attach/detach allows one to find toplevel
                                 out <- try(sys.source(the_script, envir=e), silent=TRUE)
                                 detach(e)

                                 if(inherits(out, "try-error")) {
                                   stop(out)
                                 } else {
                                   cmd <- sprintf("var session_id='%s';", session_id)
                                   toplevel$js_queue$push(cmd)
                                   ## returns javascript commands
                                   x <- toplevel$js_queue$flush()
                                 }
                               } else {
                                 ## What else goes here?
                                 x <- sprintf("File '%s' does not exist", the_script)
                                 stop(x)
                               }
                               return(x)
                             },
                             do_authentification=function(req, session_id) {
                               "Take care of authentification issues"
                               ## create authentification instance
                               if(!is(authenticator, "Authenticator")) {
                                 authenticator <<- Authenticator$new(app_name)
                               }
                               
                               if(!(authenticator$is_valid_cookie(req$cookies()) ||
                                    is_valid_cookie(req$cookies()))) {
                                 formVals <- read_rook_input(req)
                                 user <- getWithDefault(formVals$user_name, "")
                                 pwd <- getWithDefault(formVals$password, "")

                                 
                                 if(!authenticator$is_valid_user(user, pwd )) {
                                   out <- authenticator$create_login(session_id)
                                   return(out)
                                 } else {
                                   login_cookie <<- create_login_cookie(user)
                                   return(NULL)
                                 }
                               } else {
                                 return(NULL)
                               }                               
                             },
                             ##
                             create_login_cookie=function(user) {
                               l <- list("user"=user,
                                         time=as.numeric(julian(Sys.time())),
                                         value=digest(paste(user, cookie_salt))
                                         )
                               ## use JSON to serialize a list to a character
                               toJSON(l)
                             },
                             is_valid_cookie=function(cookie) {
                               "is the gWidgetsWWW2 cookie valid?"
                               l <- fromJSON(cookie$gWidgetsWWW2Login)
                               ifelse(digest(paste(l$user, cookie_salt)) == l$value &&
                                      as.numeric(julian(Sys.time())) - l$time < login_cookie_expiry,
                                      TRUE, FALSE)
                             }
                             ))


##################################################

## handle AJAX calls back into the game
GWidgetsAppAjax <- setRefClass("GWidgetsAppAjax",
                            contains="GWidgetsAppBase",
                            fields=list(
                              session_manager="ANY"
                              ),
                            methods=list(
                              initialize=function( ...) {
                                initFields(
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
                               
                               headers <- list('Content-Type'='application/javascript')
                               e_cookies <- new.env()

                               ## Here we dispatch on the path_info
                               ## value. Depending on the type we
                               ## might return JSON or javascript so
                               ## we may modify the headers
                               if(grepl("runTransport$", req$path_info())) {
                                 ## run the transport
                                 out <- run_transport(req)
                               } else if(grepl("runHandler$", req$path_info())) {
                                 ## handler, return javascript queue
                                 out <- run_handler(req, e_cookies)
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
                                  out <- "call with something"
                                }

                               ## need to populate result
                               res <- Response$new(status=200L,
                                                   headers=headers,
                                                   body=paste(out, collapse="\n")
                                                   )
                               res$write("")
                               res$finish()
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
                             
                             ## We bypass here req$GET() and req$POST() as they
                             ## have issues with characters like "&" and "+" for various
                             ## reasons. We instead pass in JSON encoded objects through the
                             ## the Ajax calls and read them directly here. We return a list so
                             ## that we can use $ for extraction
                             read_rook_input = function(req) {
                               "Read rook.input, then convert from JSON"
                               req$env[['rook.input']]$rewind()
                               input <- req$env[['rook.input']]$read()
                               l <- fromJSON(rawToChar(input))
                               if(!is.list(l))
                                 l <- sapply(l, identity, simplify=FALSE)
                               l
                             },
                             
                             ## The run functions (transport, handler, proxy
                             ## These use GET, not POST, although the latter would
                             ## be appropriate for some.
                             run_transport = function(req) {
                               "Assign values through transport. Return js commands if needed"
                               l <- read_rook_input(req)
                               toplevel <- get_toplevel(l$session_id)
                               toplevel$call_transport(l$id, l$param)
                               
                               return(toplevel$js_queue$flush())
                             },

                             
                             run_handler = function(req, e_cookies) {
                               "Run a handler. Return js commands if needed"

                               l <- read_rook_input(req)
                               
                               ## l has components id, value. Value is json
                               toplevel <- get_toplevel(l$session_id)
                               toplevel$call_handler(l$id, l$signal, l$value, e_cookies)

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

                               l <- read_rook_input(req)
                                                              
                               toplevel <- get_toplevel(l$session_id)
                               toplevel$call_rpc(l$id, l$meth, l$value)

                               return(toplevel$js_queue$flush())
                             },
                             
                             run_proxy = function(req) {
                               "Call proxy object to return JSON data"
                               
                               l <- req$GET()
                               if(length(l) == 0)
                                 stop("No info to run proxy")
                               
                               if(req$post()) {
                                 ## We bypass the whole parsing in ROok, as we pass in a JSON encoded string
                                 l$post_data <- read_rook_input(req)
                               } 

                               ## l has components session_id, id, params, param is json
                               id <- l$id; l$id <- NULL

                               
                               session_id <- l$session_id; l$session_id <- NULL
                               l[['_dc']] <- NULL # ext variable
                               toplevel <- get_toplevel(session_id)

                               if(req$post()){
                                 ##out <- toplevel$call_post_proxy(id, l, req$POST()) # return JSON
                                 out <- toplevel$call_post_proxy(id, l) # return JSON
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
                               out <- toplevel$call_upload(id, l, req$POST()) # return JSON
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

