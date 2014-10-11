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
## Can use object in global workspace (for running under Rook) or filehash (well filehash inspired) -- if running under RApache
SessionManager <- setRefClass("SessionManager",
                              fields=list(
                                "url" = "character",
                                "sessions"="list"
                                ),
                              methods=list(
                                initialize = function(...) {
                                  ## If using Rook (and perhaps nginx to proxy) -- a single process -- then
                                  ## a list suffices
                                  sessions <<- list()
                                  callSuper(...)
                                },
                                get_id = function(...) {
                                  "Create a unique new sessionID"
                                  make_ID <- function() paste(sample(LETTERS, 10, replace=TRUE), collapse="")
                                  x <- make_ID()
                                  while(x %in% names(sessions))
                                      x <- make_ID()
                                  x
                                },
                                store_session = function(id, e) {
                                  "store session"

                                  if(is.null(id))
                                    return()

                                  rec <- list(e=e,
                                              last.access=Sys.time())
                                  sessions[[id]] <<- rec
                                },
                                clear_session = function(id) {
                                  "clean up session, called when page is closed"
##                                  message("clear session ", id)
                                  
                                  sessions[[id]] <<- NULL
                                },
                                is_session_id =  function(id) {
                                  "Is id a valid sessin id?"
                                  !is.null(sessions[[id]])
                                },
                                get_session_by_id = function(id="") {
                                  "Get session, an environment. Return NULL if not there"

                                  if(is.null(id))
                                     return(NULL)

                                  rec <- sessions[[id]]

                                  e <- rec$e
                                  return(e)
                                }
                                ))

## Instead of using filehash, we borrow its ideas.
## This gives a slight speed up in a place where we really need it
SessionManagerFile <- setRefClass("SessionManagerFile",
                                  fields=list(
                                    session_dir="character"
                                    ),
                                  methods=list(
                                    initialize=function(d="/tmp/sessions", ...) {
                                      if(length(list.dirs(d)) == 0)
                                        dir.create(d)
                                      initFields(session_dir = d)
                                      callSuper(...)
                                    },
                                    make_file = function(id) {
                                      sprintf("%s%s%s", session_dir, .Platform$file.sep, id)
                                    },
                                    lock_file_name = function(id) {
                                      sprintf("%s_lock", make_file(id))
                                    },
                                    is_locked = function(id) {
                                      file.exists(lock_file_name(id))
                                    },
                                    lock_file = function(id) {
                                      cat("lock", file=lock_file_name(id))
                                    },
                                    unlock_file = function(id) {
                                      unlink(lock_file_name(id))
                                    },
                                    pkg_info=function() {
                                      "List loaded pakages. From sessionInfo()"
                                      package <- grep("^package:", search(), value = TRUE)
                                      keep <- sapply(package, function(x) x == "package:base" || 
                                                     !is.null(attr(as.environment(x), "path")))
                                      package <- sub("^package:", "", package[keep])
                                      pkgDesc <- sapply(package, packageDescription, simplify=FALSE)
                                      names(Filter(function(i) is.null(i$Priority) || i$Priority != "base", pkgDesc))
                                    },
                                    id_exists=function(id) {
                                      file.exists(make_file(id))
                                    },
                                    get_id=function() {
                                      id <- paste(sample(c(LETTERS,0:9), 10, replace=TRUE), collapse="")
                                      while(id %in% file.exists(make_file(id)))
                                        id <- make_ID()
                                      id ## check for repeats
                                    },
                                    get_session_by_id=function(id) {
                                      if(!id_exists(id))
                                        return(NULL)

                                      if(!is_locked(id)) {
                                        lock_file(id)
                                        e <- try(readRDS(make_file(id)), silent=TRUE)
                                        if(!inherits(e, "try-error")) {
                                          pkgs <- e$.sessionInfo
                                          sapply(names(pkgs), require, character.only=TRUE)
                                        }
                                        return(e)
                                      }
                                      ## else we work
                                      ctr <- 0; MAX_CT <- 10000 # how high should this be?
                                      while(is_locked(id) && ctr < MAX_CT) {
                                        Sys.sleep(0.1)
                                        ctr <- ctr + 1
                                      }
                                      if(ctr >= MAX_CT) {
                                        message("*** Failed to get lock after ***", ctr)
                                        NULL
                                      } else {
                                        lock_file(id)
                                        e <- readRDS(make_file(id))
                                        ## read in packages.
                                        ## Idea from sessionTools of Matthew D. Furia <matt.furia@sagebase.org> 
                                        pkgs <- e$.sessionInfo
                                        sapply(names(pkgs), require, character.only=TRUE)
                                        e
                                      }
                                    },
                                 store_session=function(id, e) {
                                   on.exit(unlock_file(id))
                                   e$.sessionInfo <- pkg_info()
                                   saveRDS(e, make_file(id), compress=FALSE)
                                 },
                                 clear_session=function(id) {
                                   on.exit(unlock_file(id))
                                   unlink(make_file(id))
                                 }
                                 ))



make_session_manager <- memoise(function(use.filehash=FALSE, ...) {
  if(use.filehash) {
    SessionManagerFile$new(...)
  } else {
    SessionManager$new(...)
  }
})


## Package global instance of the session manager class
## session_manager <- SessionManager$new()



