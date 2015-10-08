#      Copyright (C) 2011  John Verzani
#      Copyright (C) 2015  Johannes Ranke (port to R6)
#  
#      This program is free software: you can redistribute it and/or modify
#      it under the terms of the GNU General Public License as published by
#      the Free Software Foundation, either version 3 of the License, or
#      (at your option) any later version.
#  
#      This program is distributed in the hope that it will be useful,
#      but WITHOUT ANY WARRANTY; without even the implied warranty of
#      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#      GNU General Public License for more details.
#  
#      You should have received a copy of the GNU General Public License
#      along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' @include array.R
NULL

#' A toplevel object for a gWidgetsWWW2 app to store page-specific values
#'
#' The toplevel object for gWidgetsWWW2 apps stores page specific
#' information. There can potentially be several \code{gwindow}
#' instances on a page (using \code{renderTo}), however all share the
#' same toplevel instance. This instance holds a reference to the
#' evaluation environment, the request that generates the page, the
#' objects populating the page, and a queue for holding JavaScript
#' commands (produced by method calls, say).
#'
#' The sharing of this toplevel object among each component is the
#' reason why the constructors require a specification of either a
#' \code{container} or \code{parent} argument.
#'
#' @docType class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @importFrom R6 R6Class
#' @field The evaluation environment
#' @field The request
#' @field objects Objects on the page as an \code{\link{Array}} object
#' @field js_queue JavaScript queue to flush as an \code{\link{Array}} object
#' @field js_queue JavaScript queue to flush as an \code{\link{Array}} object
#' @field cookies An environment holding cookies
#' @field do_layout_cmd The command used for the layout
#' @name GWidgetsTopLevel-class
GWidgetsTopLevel <- R6Class("GWidgetsTopLevel",
  public = list(
    "e" = NULL,
    "the_request" = NULL,
    "objects" = NULL,
    "js_queue" = NULL,
    "cookies" = NULL,
    "do_layout_cmd" = NULL,
    initialize = function(req = NULL, ...) {
      self$the_request <- req
      self$objects <- Array$new()
      self$js_queue <- Array$new()
      self$cookies <- new.env() # replace each handler call
    },
    set_e = function(e) {
      self$e <- e
    },
    ##
    ## Call things
    ##
    call_handler = function(id, signal, params, e_cookies) {
      "lookup widget by id, notify any observer of this signal"
      self$cookies <- e_cookies
      obj <- self$get_object_by_id(id)

      out <- NULL
      if(!missing(params)) {
        ## XXX do this to stuff in things like value, index, ..
        out <- obj$before_handler(signal, params)
      }
      obj$notify_observers(signal=signal, params, out)
    },
    ## transport add javascript
    call_transport = function(id, param) {
      "Run transport"
      param <- as.list(param)
      obj <- self$get_object_by_id(id)
      do.call(obj$process_transport, param)
    },
    ## call remote procedure call
    call_rpc = function(id, meth, value) {
      obj <- self$get_object_by_id(id)
      obj$call_rpc(meth, value) ## value is a meth
    },
    ## proxy call
    call_proxy = function(id, param) {
      "Run proxy, return JSON encoded object"
      ## post might be NULL or a list where the names are important
      obj <- self$get_object_by_id(id)
      param <- as.list(param)

      ## obj should be GWidgetProxy
      out <- ""
      if(is(obj, "GWidgetProxy"))
       out <- do.call(obj$get_json_data, param)
      else
        out <- obj$get_json_data(param) # try anyways
      return(out)
    },
    call_post_proxy = function(id, param) {
      "Run proxy, return JSON encoded object"
      ## post might be NULL or a list where the names are important

      obj <- self$get_object_by_id(id)
      param <- as.list(param)
      
      ## obj should be GWidgetProxy
      out <- ""
      if(is(obj, "GWidgetProxy"))
         out <- obj$post_json_data(param)
      return(out)
    },
    
    ## upload a file
    call_upload = function(id, param, post_data) {
      "process a file upload, sets value via svalue if file exists"

      ## XXX we need to work on the handling of post data
      obj <- self$get_object_by_id(id)

      l <- Filter(is.list, post_data)
      ## list with tempfile, filename
      fname <- l[[1]]$tempfile

      if(file.exists(fname)) {
        obj$set_value(fname)
        obj$set_filename(l[[1]]$filename)
        ## aok
        out <- list(success=TRUE,
                    responseText="File uploaded"
                    )
      } else {
        out <- list(success=FALSE,
                    responseText="File uploaded",
                    errors=list(
                      portOfLoading="No file transmitted"
                      )
                    )
      }
      out
    },
    ##
    ## Method for objects. The toplevel
    ## stores all its ancestors in the
    ## objects array. This is how one
    ## interactes with them
    ##
    get_toplevel=function() {
      self
    },
    ## add the object
    add_object = function(obj, id) {
      "Add object to objects list"
      self$objects$push(obj, id)
    },
    get_object_by_id=function(id) {
      "Lookup object by its id"
      self$objects$get_by_name(id)
    },
    ## What does this do??? Used in ext-base
    get_object_id = function() {
      self$objects$get_id()
    },
    ##
    ## js_stuff
    ##
    js_queue_push = function(x) {
      "push command (string) to queue for JavaScript Commands"
      self$js_queue$push(x)
    },
    js_queue_flush = function() {
      "Flush JavaScript queue"
      self$js_queue$flush()
    },
    ## XXX not used!
    do_layout = function() {
      "Update layout from toplevel"
      if(length(nchar(do_layout_cmd))) # update layout
        self$js_queue_push(self$do_layout_cmd)
    }
  )
)
