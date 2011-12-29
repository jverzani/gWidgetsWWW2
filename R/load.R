##' @include utils.R
##' @include gwidgets-app.R
NULL

## scripts to load apps

##' load a web app defined by a gWidgetsWWW2 script
##'
##' @param script_name path to gWidgetssWWW2 script
##' @param app_name base name for script, unique per project
##' @param brew_template The script may render to the entire page, or
##' parts of the page specified by an ID. The allows one to specify a
##' brew template to hold these place holders and other HTML code.
##' @param use.googlemap If using \code{ggooglemaps} include this so that the JavaScript files are downloaded.
##' @param show.log If TRUE, logged information is written to the console
##' @param port Initial port for Rhttpd server, provided it hasn't already started.
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
                     use.googlemap = FALSE,
                     show.log=FALSE,
                     port=9000,
                     ...
                         ) {

  options("Rhttpd_debug"=as.logical(show.log))
  
  
  R <- Rhttpd$new()
  try(R$start(port=port), silent=TRUE)

  ## extra html code googlemaps, ace?, ...
  extra_html_code <- character(0)
  if(use.googlemap)
    extra_html_code <- c(extra_html_code,
                         '<script type="text/javascript" ',
                         'src="http://www.google.com/jsapi?autoload={\'modules\':[{name:\'maps\',version:3,other_params:\'sensor=false\'}]}">',
                         '</script>',
                         '<script type="text/javascript" src="/custom/gWidgetsWWW2/javascript/ext.ux.GMapPanel3.js"></script>'
                         )

  ## XXX Set up for using google visualization through googleVis. First attempt didn't work.
  ## if(use.googleVis)
  ##   extra_html_code <- c(extra_html_code,
  ##                        '<script type="text/javascript" src="http://www.google.com/jsapi"></script>'
  ##                        )
                         

  
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
                           tmpApp,
                           GWidgetsApp$new(url="/gwapp", script=script_name, session_manager=make_session_manager()),
                           ## why does this fail?
                           Rook:::Redirect$new(sprintf("http://127.0.0.1:%s/custom/%s/indexgw.rhtml", tools:::httpdPort, app_name))
                           )
  
  R$add(RhttpdApp$new(app, name=app_name))
  
  browseURL(sprintf("http://127.0.0.1:%s/custom/%s/indexgw.rhtml", tools:::httpdPort, app_name))
  
}

