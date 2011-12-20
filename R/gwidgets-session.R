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

##' Session manager for gWidgetsWWW2 apps
SessionManager <- setRefClass("SessionManager",
                              fields=list(
                                "url" = "character",
                                "sessions"="list"
                                ),
                              methods=list(
                                initialize = function(...) {
                                  sessions <<- list()
                                  callSuper(...)
                                },
                                get_id = function(...) {
                                  "Create a unique new sessionID"
                                  make_ID <- function() paste(sample(LETTERS, 10, replace=TRUE), collapse="")
                                  x <- make_ID()
                                  while(x %in% names(sessions))
                                    x <- make_ID()
                                  return(x)
                                },
                                store_session = function(id, e) {
                                  "store session"
                                  rec <- list(e=e,
                                              last.access=Sys.time())
                                  ## XXX
                                  if(!is.null(id))
                                    sessions[[id]] <<- rec
                                },
                                clear_session = function(id) {
                                  "clean up session, called when page is closed"
                                  sessions[[id]] <<- NULL
                                },
                                ## Not used!!!
                                new_session = function() {
                                  "Create new session, return the ID"
                                  id <- get_id()
                                  e <- new.env()
                                  ## JV ??? Why isn't toplevel stored anywhere?
                                  toplevel <- GWidgetsTopLevel$new(id)
                                  store_session(id, e)
                                  return(id)
                                },
                                get_session_by_id = function(id="") {
                                  "Get session, an environment. Return NULL if not there"
                                  if(is.null(id))
                                    return(NULL)
                                  rec <- sessions[[id, exact=TRUE]]
                                  if(is.null(rec)) {
                                    return(NULL)
                                  }

                                  
                                  rec$last.access <- Sys.time()
                                  sessions[[id]] <<- rec

                                  e <- rec$e
                                  if(!is.environment(e)) {
                                    ## e has been serialized
                                    e <- get_serialized_session(id)
                                  }
                                  return(e)
                                },
                                get_serialized_session = function(id) {
                                  "Get serialized session by id"
                                  ## use filehash, ...
                                  cat("XXX imlement me")
                                }

                                ))

make_session_manager <- memoise(function() {
  SessionManager$new()
})


## Package global instance of the session manager class
## session_manager <- SessionManager$new()



