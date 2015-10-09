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

#' @include utils.R
#' @include gwidgets-app.R
NULL

# scripts to load apps

#' The help server as an R6 class
#' 
#' @importFrom Rook Rhttpd RhttpdApp Static
#' @fields R An Rhttpd object
#' @fields port The port of the Dynamic help server
#' @fields started Is the R_http instance running?
#' @fields loaded Is the gWidgets infrastructure loaded?
R_http <- R6Class("R_http",
  public = list(
    "R" = NULL,
    "port" = NULL,
    "started" = FALSE,
    "loaded" = FALSE,
    initialize=function(...) {
      self$R = Rhttpd$new()
    },
    start = function() {
        if(self$started)
            return()
        self$port <- tools::startDynamicHelp(start = NA)
        self$started <- TRUE
        return()
    },
    load_gw = function(session_manager = make_session_manager()) {
      if(self$loaded) return()
      ## gWidgetsWWW, static files
      gWidgetsWWW <- Rook::Static$new(
        urls = c("/images", "/javascript", "/css"),
        root = system.file("base", package="gWidgetsWWW2")
      )
      self$R$add(RhttpdApp$new(gWidgetsWWW, name="gWidgetsWWW2"))
           
      ## tmpdir
      tmpApp <- Rook::Static$new(
        urls=c("/tmp"),
        root=tempdir()
      )
      self$R$add(RhttpdApp$new(tmpApp, name="tmp"))
           
      ## This handles the AJAX calls
      self$R$add(GWidgetsAppAjax$new(session_manager=session_manager), 
                 name="gwappAJAX")

      self$loaded <- TRUE
    },
    load_app = function(script_name,
                        app_name = gsub("\\..*", "", basename(script_name)),
                        session_manager = make_session_manager(),
                        open_page = TRUE, show.log = FALSE, webpage = NULL, 
                        # authenticator=NULL,
                        ...)  {

      options("Rhttpd_debug" = as.logical(show.log))

      ## Make the page
      if(is.null(webpage))
        webpage <-WebPage$new(url="/", app_name=app_name, ...)
      else
        webpage$app_name <- app_name
    
      self$R$add(RhttpdApp$new(webpage, name=app_name))
      
      ## message(list("*** making app ***", app_name))
      
      ## Make the app
      gwapp <- gWidgetsWWW2:::GWidgetsApp$new(url="/",
                                              app_name = app_name,
                                              script = script_name,
                                              session_manager = session_manager)
      self$R$add(RhttpdApp$new(gwapp, name=sprintf("app_%s", app_name)))
      
      if(open_page) {
        ## message("load page, port:", port, "app name: ", app_name)
        browseURL(sprintf("http://127.0.0.1:%s/custom/%s", port, app_name))
      }
      invisible(gwapp)
    }
  )
)

#' Load a web app defined by a gWidgetsWWW2 script
#'
#' There are two basic tasks that gWidgetsWWW2 does: a) create the
#' javascript to populate a page (or part of the page) and b) create
#' a means for AJAX requests between the web browser and the R
#' process. For b) there isn't much to say except that it is supposed
#' to just work.
#'
#' As for a) there are things one can pay attention to. The simplest
#' case is really pretty simple. Write some script, save it to a file
#' and call this function with the script name (and path if needed).
#' This simple case will create a full screen app. Each app runs in
#' its own environment in the R session, so running lots of instances
#' can be expensive. That is, don't expect gWidgetsWWW2 to scale.
#'
#' To embed an app within a web page, use an \code{iframe} tag.
#'
#' @param script_name path to gWidgetsWWW2 script
#' @param app_name base name for script, unique per project. Derived
#' from script name if not specified.
#' @param show.log If TRUE, logged information is written to the console.
#' @param port Initial port for Rhttpd server, provided it hasn't already started.
#' @param session_manager an instance of \code{make_session_manager}
#' @param open_page logical. If \code{TRUE} call \code{browseURL} to open the app.
#' @param ... Passed to WebPage object. Named arguments
#' \code{body} and \code{head} can be used to insert information into
#' each, such as loading of style sheets etc. through \code{head}. One can 
#' also subclass \code{WebPage} and pass this in via \code{webpage}.
#' @export
#' @examples
#' ## open an app that takes the entire page
#' gw_script <-  system.file("examples/ex-hello-world.R", package="gWidgetsWWW2")
#' if(interactive()) load_app(gw_script, "HelloApp")
load_app <- function(script_name, app_name = NULL, port = NULL,
                     session_manager = make_session_manager(),
                     open_page = TRUE, show.log = FALSE, ...)
{
  if (!file.exists(script_name))
    stop(sprintf("Script %s does not exist", script_name))
    
  if(is.null(app_name))
    app_name <- gsub("\\..*", "", basename(script_name))

  if (!is.null(port))
    options(help.ports=port)
    r_httpd <- R_http$get_instance()
    r_httpd$start()                   # if not started
    r_httpd$load_gw(session_manager)
    ## now load app
    r_httpd$load_app(script_name, app_name, session_manager, open_page,  show.log,
                     ##authenticator,
                     ...)
}
