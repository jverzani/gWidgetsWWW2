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

#' @include gwidgets-package.R
NULL

#' An array class.
#'
#' Like a list, but has some methods. Completely superflous, but
#' makes copying some code algorithms easier. We implement methods
#' such as \code{append}, \code{push}, \code{pop} and \code{each} for
#' iteration. As well, there are some lookup methods.
#' @importFrom R6 R6Class
#' @docType class
#' @export Array
#' @format An \code{\link{R6Class}} generator object
#' @field l The list where the key value pairs are stored
#' @field id_ctr An integer counter for getting new names (as character strings)
#' @name Array-class
Array <- R6Class("Array",
  public = list(
    l = list(),
    id_ctr = integer(),
    initialize = function(...) {
      if(nargs()) {
        if(is.list(..1))
          self$l <- ..1
        else
          self$l <- list(...)
      } else {
        self$l <- list()
      }
      
      self$id_ctr <- 0L
      invisible(self)
    },
    core = function() {
      "return list"
      self$l
    },
    flush = function(...) {
      "Reset array, return contents as list"
      tmp <- self$l
      self$l <- list()
      tmp
    },
    get_id = function() {
      "Return an id, or name, for an object"
      self$id_ctr <- self$id_ctr + 1L
      sprintf("%s", self$id_ctr)
    },
    contains = function(name) {
      "TRUE if name is key in array"
      !is.null(self$get_by_name(name))
    },
    contains_item = function(item) {
      length(Filter(function(x) identical(x, item), self$l)) > 0
    },
    get_by_name = function(name) {
      "get item under name"
      self$l[[name, exact=TRUE]]
    },
    get_item = function(index) {
      "Get item by index"
      self$l[[index]]
    },
    append = function(...) {
      ## append values to array
      if(nargs()) {
        if(is.list(..1))
          lst <- ..1
        else
          lst <- list(...)
        nms <- names(lst)
        if (!is.null(nms)) {
          sapply(seq_along(lst), function(i) {
            self$push(lst[[i]], nms[i])
          })
        } else {
          sapply(seq_along(lst), function(i) {
            self$push(lst[[i]])
          })
        }
        invisible()
      }
    },
    push = function(x, name) {
      "Append x with optional name. If name not specified new id created. Returns name"
      if(missing(name)) name <- self$get_id()
      self$l[[name]] <- x
      name
    },
    pop = function() {
      out <- self$l[length(self$l)]
      self$l <- self$l[-length(self$l)]
      out
    },
    remove_by_name = function(name) {
      ## remove item by id key
      self$l[[name]] <- NULL
    },
    len = function() {
      "length"
      base:::length(self$l)
    },
    each = function(FUN, ...) {
      "Iterator for lists, like sapply, but FUN gets passed index, key, and value"
      nms <- names(self$l)
      sapply(seq_along(self$l), 
             function(i, ...) {
               key <- nms[i]
               value <- self$l[[i]]
               FUN(i, key, value, ...)
             }, ...)
    },
    pluck = function(id, ...) {
      "Like ext.pluck. Returns array with 'id' extracted from each item in the Array"
      sapply(self$l, function(i) i[[id]])
    }
  )
)
