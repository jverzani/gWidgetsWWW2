##      Copyright (C) 2011  John Verzani
##      Copyright (C) 2015  Johannes Ranke (port to R6 and removed Apache support)
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
## Uses object in global workspace for running under Rook
SessionManager <- R6Class("SessionManager",
  public = list(
    url = NULL,
    sessions = list(),
    initialize = function(...) {
      self$sessions <- list()
    },
    get_id = function(...) {
      "Create a unique new sessionID"
      make_ID <- function() paste(sample(LETTERS, 10, replace=TRUE), collapse="")
      x <- make_ID()
      while(x %in% names(self$sessions))
          x <- make_ID()
      x
    },
    store_session = function(id, e) {
      "store session"

      if(is.null(id))
        return()

      rec <- list(e=e,
                  last.access=Sys.time())
      self$sessions[[id]] <- rec
    },
    clear_session = function(id) {
      "clean up session, called when page is closed"
      ##  message("clear session ", id)
      
      self$sessions[[id]] <- NULL
    },
    is_session_id =  function(id) {
      "Is id a valid sessin id?"
      !is.null(self$sessions[[id]])
    },
    get_session_by_id = function(id="") {
      "Get session, an environment. Return NULL if not there"

      if(is.null(id))
         return(NULL)

      rec <- self$sessions[[id]]

      e <- rec$e
      return(e)
    }
  )
)

# JR: Memoisation should improve speed, does it?
make_session_manager <- memoise(function(...) {
  SessionManager$new(...)
})
