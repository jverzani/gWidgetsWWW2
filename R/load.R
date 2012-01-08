##' @include utils.R
##' @include gwidgets-app.R
NULL

## scripts to load apps

##' Load a web app defined by a gWidgetsWWW2 script
##'
##' There are two basic tasks that gWidgetsWWW2 does: a) create the
##' javascript to populate a page (or part of the page) and b) create
##' an means for AJAX requests between the web browser and the R
##' process. For b) there isn't much to say except that it is supposed
##' to just work.
##'
##' As for a) there are things one can pay attention to. The simplest
##' case is really pretty simple. Write some script, save it to a file
##' and call this function with the script name (and path if needed).
##' This simple case will create a full screen app. Each app runs in
##' its own environment in the R session, so running lots of instances
##' can be expensive. That is, don't expect gWidgetsWWW2 to scale.
##'
##' There are other cases one might want. An app can run within a web
##' page. The web template can be passed through the
##' \code{brew_template} argument with the \code{...} parameters
##' passed along.
##'
##' One might even be able to copy the provided indexgw.rhtml file and
##' modify that. When using either of these approaches, pass a DOM id
##' to the \code{renderTo} argument of the \code{gwindow} instance.
##'
##' Apps can require authentification. By default, this is not
##' validated. One can write a subclass of \code{Authenticator} to do
##' other validations. (Of course this needs improvement).
##' 
##' @param script_name path to gWidgetssWWW2 script
##' @param app_name base name for script, unique per project
##' @param brew_template The script may render to the entire page, or
##' parts of the page specified by an ID. The allows one to specify a
##' brew template to hold these place holders and other HTML code.
##' @param show.log If TRUE, logged information is written to the console
##' @param port Initial port for Rhttpd server, provided it hasn't already started.
##' @param authenticator Used to specify a subclass of
##' \code{\link{Authenticator}} for doing authentification. Specifying
##' a value of \code{TRUE} will do a trivial
##' authentification. Subclasses of \code{Authenticator} should
##' implement the methods \code{is_valid_cookie} and
##' \code{is_valid_user}.
##' @param ... passed to \code{brew} call of \code{brew_template}
##' @export
##' @examples
##' ## open an app that takes the entire page
##' gw_script <-  system.file("examples/hello-world.R", package="gWidgetsWWW2")
##' if(interactive()) load_app(gw_script, "HelloApp",  use.google=FALSE)
##' ## open an app using googlemaps
##' if(interactive()) load_app(system.file("examples/ggooglemaps.R", package="gWidgetsWWW2"),  "GoogleMapApp", use.google=TRUE)
##' ## open an app embedded in another page
##' gw_script <- system.file("examples/ex-multiple-gwindow.R", package="gWidgetsWWW2")
##' brew_template <- system.file("framework/brew/custom.rhtml", package="gWidgetsWWW2")
##' if(interactive()) load_app(gw_script, "MultipleApp",  brew_template)
##' 
load_app <- function(script_name,
                     app_name="test",
                     brew_template = "",
                     show.log=FALSE,
                     port=9000,
                     authenticator=NULL,
                     ...
                         ) {

  load_once(port=port)

  options("Rhttpd_debug"=as.logical(show.log))
  
  R <- Rhttpd$new()
  try(R$start(port=port), silent=TRUE)

  ## extra html code googlemaps, ace?, ...
  extra_html_code <- character(0)
                         
  ## brew index
  brewery <- Rook:::Brewery$new(url="/",
                                root=system.file("framework/brew", package="gWidgetsWWW2"),
                                app_name = app_name,
                                brew_template=brew_template,
                                extra_html_code = paste(extra_html_code, collapse="\n"),
                                ...
                                )
                                        #  R$add(RhttpdApp$new(brewery, name="gwbrew"))

  
  ## an application
  gwapp <- GWidgetsApp$new(url="/gwapp", app_name=app_name, script=script_name,
                           session_manager=make_session_manager(),
                           authenticator=authenticator
                           )
  app <- Rook::Builder$new(
                           ## app specific static files
                           Rook:::Static$new(
                                             urls = c('/css','/images','/javascript'), 
                                             root = '.'
                                             ),
                           ## brew files
                           brewery,
                           ## Rook:::Brewery$new(
                           ##                    url=sprintf("%s/brew", app_name),
                           ##                    root='.'
                           ##                    ),
                           ## why do I need this?
#                           tmpApp,
                           gwapp,
                           ## why does this fail?
                           Rook:::Redirect$new(sprintf("http://127.0.0.1:%s/custom/%s/indexgw.rhtml", tools:::httpdPort, app_name))
                           )
  
  R$add(RhttpdApp$new(app, name=app_name))
  
  browseURL(sprintf("http://127.0.0.1:%s/custom/%s/indexgw.rhtml", tools:::httpdPort, app_name))
  invisible(gwapp)
}

.load_once <- function(port=9000, ...) {
  
  R <- Rhttpd$new()
  try(R$start(port=port), silent=TRUE)

  
  
  ## gWidgetsWWW, static files
  gWidgetsWWW <- Rook::Static$new(
                                  urls = c("/images", "/javascript", "/css"),
                                  root = system.file("base", package="gWidgetsWWW2")
                                  )
  R$add(RhttpdApp$new(gWidgetsWWW, name="gWidgetsWWW2"))
  
  ## tmpdir
  tmpApp <- Rook::Static$new(
                             urls=c("/tmp"),
                             root=tempdir()
                             )
  R$add(RhttpdApp$new(tmpApp, name="tmp"))
  

  ## This handles the AJAX calls
  R$add(GWidgetsAppAjax$new(session_manager=make_session_manager()), name="gwappAJAX")

}

## internal helper function to load static files for gWdigetsWWW2
load_once <- memoize(.load_once)
