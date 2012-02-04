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
##' \code{\link{Authenticator-class}} for doing authentification. Specifying
##' a value of \code{TRUE} will do a trivial
##' authentification. Subclasses of \code{Authenticator} should
##' implement the methods \code{is_valid_cookie} and
##' \code{is_valid_user}.
##' @param session_manager an instance of \code{make_session_manager}
##' @param open_page logical. If \code{TRUE} call \code{browseURL} to open the app.
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
                     app_name=gsub("\\..*", "", basename(script_name)),
                     session_manager=make_session_manager(),
                     R_httpd=Rhttpd$new(),
                     open_page=TRUE,
                     brew_template = "",
                     show.log=FALSE,
                     authenticator=NULL,
                     ...
                         ) {

  options("Rhttpd_debug"=as.logical(show.log))

  ## Make the page
  page <-WebPage$new(url="/", app_name=app_name)
  ## how to subclass to put in brew_template
  if(file.exists(brew_template)) {
     BrewPage <- setRefClass("BrewPage",
                             contains="WebPage",
                             fields=c("brew_template"),
                             methods=list(
                               body=function() {
                                 brew_template
                               }
                               ))$new(url="/", app_name=app_name, renderer="brew", brew_template=brew_template)
   }
  R_httpd$add(RhttpdApp$new(page, name=app_name))

  print(list("*** making app ***", app_name))
  
  ## Make the app
  gwapp <- gWidgetsWWW2:::GWidgetsApp$new(url="/",
                                          app_name=app_name,
                                          script=script_name,
                                          session_manager=session_manager)
  R_httpd$add(RhttpdApp$new(gwapp, name=sprintf("app_%s", app_name)))
  
  if(open_page)
    browseURL(sprintf("http://127.0.0.1:%s/custom/%s", tools:::httpdPort, app_name))
  
  invisible(gwapp)
}

##' Load an app in a directory
##'
##' @param dirname The directory name (with full path). We make several assuptions:
##' 1) \code{dir/*.R} has one answer, or we take \code{index.R}, or we take first alphabetically;
##' 2) \code{app_name} is \code{basename(dir_name)};
##' 3) \code{brew/*rhtml} is empty (fullscreen), has one value, or has \code{index.rhtml} or we take first alphabetically;
##' 4) authenticator (XXX to be determined, for now calls \code{make_session_manager})
##' @export
##' @return creates the app
load_dir <- function(dir_name, ...) {
  sep <- .Platform$file.sep
  dir_name <- gsub(sprintf("%s*$", sep), "", dir_name)

  app_name <- basename(dir_name)
  
  r_files <- Sys.glob(sprintf("%s%s*.R", dir_name, sep)) 
  if(length(r_files) == 0) {
    stop(gettext(sprintf("No R files found in %s", dir_name)))
  } else if(length(r_files) == 1) {
    r_file <- r_files
  } else {
    ind <- match("index.R", basename(r_files))
    r_file <- ifelse(is.na(ind), r_files[1], r_files[ind])
  }
  
  brew_files <- Sys.glob(sprintf("%s%sbrew%s*.rhtml", dir_name, sep, sep))
  if(length(brew_files) <= 1) {
    brew_file <- ""
  } else {
    ind <- match("index.rhtml", basename(brew_files))
    brew_file <- ifelse(is.na(ind), brew_files[1], brew_files[ind])
  }

  ## How to do make_session
  ## how to do Authenticator

#  cur_dir <- getwd()
#  on.exit(setwd(cur_dir))
  setwd(dir_name)
  
  
  load_app(
           script_name=r_file,
           app_name=app_name,
           brew_template = brew_file,
           ...
           )
  


}

SetupGWApps <- setRefClass("SetupGwApps",
                           fields=list(
                             "R"="ANY",
                             "port"="numeric",
                             "session_manager"="ANY",
                             "loaded"="logical"
                             ),
                           methods=list(
                             initialize=function(port=9000, session_manager, ...) {
                               initFields(
                                          R=Rhttpd$new(),
                                          port=port,
                                          session_manager=session_manager,
                                          loaded=FALSE)
                               callSuper(...)
                             },
                             start=function() {
                               try(R$start(port=port), silent=TRUE)
                             },
                             init_gw=function() {
                               if(loaded)
                                 return

                               ## gWidgetsWWW, static files
                               gWidgetsWWW <- Rook::Static$new(
                                                               urls = c("/images", "/javascript", "/css"),
                                                               root = system.file("base", package="gWidgetsWWW2")
                                                               )
                               if(is.null(R$appList[["gWidgetsWWW2"]]))
                                  R$add(RhttpdApp$new(gWidgetsWWW, name="gWidgetsWWW2"))
  
                               ## tmpdir
                               tmpApp <- Rook::Static$new(
                                                          urls=c("/tmp"),
                                                          root=tempdir()
                                                          )
                               if(is.null(R$appList[["tmp"]]))
                                  R$add(RhttpdApp$new(tmpApp, name="tmp"))
  
                               ## This handles the AJAX calls
                               if(is.null(R$appList[["gwappAJAX"]]))
                                 R$add(GWidgetsAppAjax$new(session_manager=session_manager), name="gwappAJAX")

                               loaded <<- TRUE
                             }
                             ))


## internal helper function to load static files for gWdigetsWWW2

.load_gwidgets_base <- function(port=9000L, session_manager=session_manager()) {
  Rk <- SetupGWApps$new(port=port, session_manager=session_manager)
  Rk$start()
  Rk$init_gw()
  Rk
}

load_gwidgets_base <- memoize(.load_gwidgets_base) 
