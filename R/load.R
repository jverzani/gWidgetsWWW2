##' @include utils.R
##' @include gwidgets-app.R
NULL

## scripts to load apps



## return an instance of this singleton class
R_http <- setRefClass("R_http",
                      contains="Singleton",
                      fields=c("started", "loaded", "R"), # fake, just to keep down warnings
                      methods=list(
                        initialize=function(...) {
                          Class <<- setRefClass("Class",
                                                fields=list(
                                                  "R"="ANY",
                                                  "port"="numeric",
                                                  "started"="logical",
                                                  "loaded"="logical"),
                                                methods=list(
                                                  initialize=function(...) {
                                                    initFields(started=FALSE,
                                                               loaded=FALSE,
                                                               R=Rhttpd$new()
                                                               )
                                                    callSuper(...)
                                                  },
                                                  start=function(...) {
                                                    if(started)
                                                      return()
                                                    env = environment(tools::startDynamicHelp)
                                                    cur_port <- get("httpdPort", env)
                                                    ## cur_port <- tools:::httpdPort
                                                    if(cur_port > 0) {
                                                      if(cur_port != port) {
                                                        message(sprintf("Web server already started on port %s", cur_port))
                                                        assign("port", cur_port, .self)
                                                      }
                                                      started <<- TRUE
                                                    } else {
                                                        ## pass in listen="external.ip"
                                                        ## message(sprintf("Starting server on port %s", port))
                                                        ## options(help.ports=port) ## port is failing!!!
                                                        out <- R$start(...)

                                                        env = environment(tools::startDynamicHelp)
                                                        assign("port", get("httpdPort", env), .self)
                                                        started <<- TRUE
                                                    }
                                                  },
                                                  load_gw=function(session_manager=make_session_manager()) {
                                                    if(loaded) return()
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
                                                    R$add(GWidgetsAppAjax$new(session_manager=session_manager), name="gwappAJAX")

                                                    loaded <<- TRUE
                                                  },
                                                  load_app=function(
                                                    script_name,
                                                    app_name=gsub("\\..*", "", basename(script_name)),
                                                    session_manager=make_session_manager(),
                                                    open_page=TRUE,
                                                    show.log=FALSE,
                                                    webpage = NULL, 
#                                                    authenticator=NULL,
                                                    ...)  {

                                                    options("Rhttpd_debug"=as.logical(show.log))

                                                    ## Make the page
                                                    if(is.null(webpage))
                                                      webpage <-WebPage$new(url="/", app_name=app_name, ...)
                                                    else
                                                      webpage$app_name <- app_name
                                                  
                                                    R$add(RhttpdApp$new(webpage, name=app_name))
                                                    
                                                    ## message(list("*** making app ***", app_name))
                                                    
                                                    ## Make the app
                                                    gwapp <- gWidgetsWWW2:::GWidgetsApp$new(url="/",
                                                                                            app_name=app_name,
                                                                                            script=script_name,
                                                                                            session_manager=session_manager)
                                                    R$add(RhttpdApp$new(gwapp, name=sprintf("app_%s", app_name)))
                                                    
                                                    if(open_page) {
##                                                      message("load page, port:", port, "app name: ", app_name)
                                                      browseURL(sprintf("http://127.0.0.1:%s/custom/%s", port, app_name))
                                                    }
                                                    invisible(gwapp)
                                                  },
                                                  load_dirapp=function(
                                                    dir_name, ...) {
                                                    "
##' Load an app in a directory
##'
##' @param dirname The directory name (with full path). We make several assuptions:
##' 1) `dir/*.R` has one answer, or we take `index.R`, or we take first alphabetically;
##' 2) `app_name` is `basename(dir_name)`;
##' @export
##' @return creates the app
"
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
                                                    

                                                    ## How to do make_session
                                                    ## how to do Authentification?
                                                    setwd(dir_name)
                                                    
                                                    
                                                    load_app(
                                                             script_name=r_file,
                                                             app_name=app_name,
                                                             ...
                                                             )
                                                    

                                                  }
                                                  ))
                          callSuper(...)
                        }
                        ))$new()
## put this in zzz.R
r_httpd <- R_http$get_instance()




##' Load a web app defined by a gWidgetsWWW2 script
##'
##' There are two basic tasks that gWidgetsWWW2 does: a) create the
##' javascript to populate a page (or part of the page) and b) create
##' a means for AJAX requests between the web browser and the R
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
##' To embed an app within a web page, use an \code{iframe} tag.
##'
##' @param script_name path to gWidgetssWWW2 script
##' @param app_name base name for script, unique per project. Derived
##' from script name if not specified.
##' @param show.log If TRUE, logged information is written to the console
##' @param port Initial port for Rhttpd server, provided it hasn't already started.
##' @param session_manager an instance of \code{make_session_manager}
##' @param open_page logical. If \code{TRUE} call \code{browseURL} to open the app.
##' @param ... Passed to WebPage object. Named arguments
##' \code{body} and \code{head} can be used to insert information into
##' each, such as loading of style sheets etc. through \code{head}. One can also subclass \code{WebPage} and pass this in via \code{webpage}.
##' @export
##' @examples
##' ## open an app that takes the entire page
##' gw_script <-  system.file("examples/ex-hello-world.R", package="gWidgetsWWW2")
##' if(interactive()) load_app(gw_script, "HelloApp")
load_app <- function(script_name,
                     app_name=NULL,
                     port=NULL,
                     session_manager=make_session_manager(),
                     open_page=TRUE,
                     show.log=FALSE,
                     ...
                         ) {

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

##' Load an app in a directory
##'
##' @param dir_name The directory name (with full path). We make several assuptions:
##' 1) \code{dir/*.R} has one answer, or we
##' take \code{index.R}, or we take first alphabetically; 2)
##' \code{app_name} is \code{basename(dir_name)}; 3)
##' \code{brew/*rhtml} is empty (fullscreen), has one value, or has
##' \code{index.rhtml} or we take first alphabetically; 4)
##' authenticator (XXX to be determined, for now calls
##' \code{make_session_manager})
##' @param ... passed to \code{load_dir} reference class method of
##' \code{R_http} instance.
##' @export
##' @return creates the app
load_dirapp <- function(dir_name, ...) {
  r_httpd <- R_http$get_instance()
  r_httpd$load_dirapp(dir_name, ...)
}


##' Load all apps in the directory specified
##'
##' Loads all files with "r" or "R" extension in the specified directory
##' @param dir_name directory name
##' @param session_manager session manager
##' @export
##' @return list of return values from \code{load_app}
load_dir <- function(dir_name, session_manager=make_session_manager()) {

  f <- list.files(dir_name, full.names=TRUE)
  f <- Filter(function(x) grepl("[rR]$", x), f)
  sapply(f, load_app, session_manager=session_manager, open_page=FALSE, simplify=FALSE)

}
