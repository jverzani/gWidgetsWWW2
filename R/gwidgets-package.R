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

##' @import methods 
##' @import Rook
##' @import RJSONIO
##' @import digest
##' @import memoise
##' @import whisker
NULL

##' An implementation of the gWidgets API for web programming
##'
##' This package uses the \pkg{Rook} package of Jeffrey Horner to
##' allow R programmers to easily create interactive web pages from
##' within an R session.
##'
##' Installation:
##'
##' There is a choice of web server. For serving local pages, the Rook package can use R's httpd server.
##'
##' For serving pages to a wider world one can
##'
##' * open R's httpd server's port to the wider world (not recommended)
##'
##' Just call the load_app command on a script and the page will render.
##' 
##' * proxy this through something, like nginx
##'
##' We follow the steps in J Horner's gist (https://gist.github.com/6d09536d871c1a648a84)
##'
##' 1) install nginx: sudo apt-get install nginx
##' 2) configure nginx by adding this to /etc/nginx/sites-enabled/default (in the servef bit)
##' 
##' location /custom {
##' proxy_pass http://localhost:19000/custom;
##' }
##' 
##' * run scripts under rapache
##'
##' For nginx
##'
##'
##' @name gWidgetsWWW2-package
##' @docType package
##' @author John Verzani \email{jverzani@@gmail.com}
##' @keywords package
NULL

.onLoad <- function(...) {
  ## this is needed for local files, we need an initial instance
#  r_httpd <- R_http$get_instance()

}
