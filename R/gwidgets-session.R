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

##' @include gwidgets-toplevel.R
NULL

## Session manager for gWidgetsWWW2 apps
## Can use object in global workspace (for running under Rook) or filehash -- if running under RApache
SessionManager <- setRefClass("SessionManager",
                              fields=list(
                                "url" = "character",
#                                "sessions"="list"
                                "sessions"="ANY",
                                use_filehash="logical"
                                ),
                              methods=list(
                                initialize = function(use_filehash=FALSE, ...) {
                                  initFields(use_filehash = use_filehash)
                                  if(use_filehash) {
                                    ## filehash allows us to use rapache and multiple
                                    ## R processes. It is *much* slower.
                                    db_name <- getOption("gWidgetsWWW2:db_name")
                                    if(is.null(db_name))
                                      db_name <- "/tmp/gWidgetsWWW2_session_db"
                                    dbCreate(db_name)
                                    sessions <<- dbInit(db_name)
                                  } else {
                                    ## If using Rook (and perhaps nginx to proxy) a single process then
                                    ## a list suffices
                                    sessions <<- list()
                                  }
                                  callSuper(...)
                                },
                                get_id = function(...) {
                                  "Create a unique new sessionID"
                                  make_ID <- function() paste(sample(LETTERS, 10, replace=TRUE), collapse="")
                                  x <- make_ID()
                                  if(use_filehash)
                                    while(dbExists(sessions, x))
                                      x <- make_ID()
                                  else
                                    while(x %in% names(sessions))
                                      x <- make_ID()
                                  return(x)
                                },
                                store_session = function(id, e) {
                                  "store session"

                                  if(is.null(id))
                                    return()

                                  rec <- list(e=e,
                                              last.access=Sys.time())
                                  if(use_filehash) {
                                    done <- FALSE; ctr <- 1
                                    while(!done && ctr < 10) {
                                      out <- try(dbInsert(sessions, id, rec), silent=TRUE)
                                      if(!inherits(out, "try-error"))
                                        done <- TRUE
                                      ctr <- ctr + 1
                                    }
                                    if(!done)
                                      stop(out)
                                  } else {
                                    sessions[[id]] <<- rec
                                  }
                                },
                                clear_session = function(id) {
                                  "clean up session, called when page is closed"
                                  message("clear session: ", id)
                                  
                                  if(use_filehash) {
                                    done <- FALSE;; ctr <- 1
                                    while(!done && ctr < 10) {
                                      out <- try(dbInsert(sessions, id, NULL), silent=TRUE)
                                      if(!inherits(out, "try-error"))
                                        done <- TRUE
                                      ctr <- ctr + 1
                                    }
                                    if(!done)
                                      stop(out)
                                  } else {
                                    sessions[[id]] <<- NULL
                                  }
                                },
                                new_session = function() {
                                  "Create new session, return the ID"
                                  id <- get_id()
                                  e <- new.env()
                                  store_session(id, e)

                                  message("created id: ", id)
                                  
                                  return(id)
                                },
                                get_session_by_id = function(id="") {
                                  "Get session, an environment. Return NULL if not there"

                                  message("get sessison by id: ", id)
                                  
                                  if(is.null(id))
                                     return(NULL)

                                  if(use_filehash) {
                                    ## using filehash
                                    if(!dbExists(sessions, id))
                                      return()
                                    
                                    done <- FALSE; ctr <- 1
                                    while(!done && ctr < 10) {
                                      rec <- try(dbFetch(sessions, id), silent=TRUE)
                                      if(!inherits(rec, "try-error"))
                                        done <- TRUE
                                      ctr <- ctr + 1
                                    }
                                    if(!done)
                                      stop(rec)
                                    
                                    if(is.null(rec))
                                      return(NULL)
                                  } else {
                                    ## using a list
                                    rec <- sessions[[id]]
                                  }
                                  
                                  e <- rec$e
                                  return(e)
                                }
                                ))

make_session_manager <- memoise(function(...) {
  SessionManager$new(...)
})


## Package global instance of the session manager class
## session_manager <- SessionManager$new()



