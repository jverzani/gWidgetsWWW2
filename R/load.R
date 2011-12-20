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
##' @param ... passed to \code{brew} call of \code{brew_template}
##' @export
##' @examples
##' ## open an app that takes the entire page
##' gw_script <-  system.file("examples/hello-world.R", package="gWidgetsWWW2")
##' load_app(gw_script, "HelloApp",  use.google=FALSE)
##' ## open an app using googlemaps
##' load_app(system.file("examples/ggooglemaps.R", package="gWidgetsWWW2"),  "GoogleMapApp", use.google=TRUE)
##' ## open an app embedded in another page
##' gw_script <- system.file("examples/ex-multiple-gwindow.R", package="gWidgetsWWW2")
##' brew_template <- system.file("framework/brew/custom.rhtml", package="gWidgetsWWW2")
##' load_app(gw_script, "MultipleApp",  brew_template)
load_app <- function(script_name,
                     app_name="test",
                     brew_template = "",
                     use.googlemap = FALSE,
##                     use.googleVis=TRUE,
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
  ls_str <- function(s) paste(capture.output(str(s),file=NULL),collapse='\n')
  R$add(name="request",
        app=function(env){
          req <- Request$new(env)
          res <- Response$new()
          res$write("<script type='text/javascript' src='/custom/gWidgetsWWW2/javascript/jquery-1.5.2.min.js'></script>")
          res$set_cookie('imacookie','42')
          action <- req$to_url('/foo',bar=1,baz='three')
          res$write('<form enctype="multipart/form-data" method=POST action="')
          res$write(action)
          res$write('">')
          res$write('Upload a file: <input type=file name=fileUpload>')
          res$write('<input type=submit></form><br><pre>')
          res$write(c('parseable_data: ',req$parseable_data(),'\n'))
          res$write(c('url: ',req$url(),'\n'))
          res$write(c('request_method: ',req$request_method(),'\n'))
          res$write(c('GET: ',ls_str(req$GET()),'\n'))
          res$write(c('post: ',req$post(),'\n'))
          res$write(c('media_type: ',req$media_type(),'\n'))
          res$write(c('query_string: ',req$query_string(),'\n'))
          res$write(c('fullpath: ',req$fullpath(),'\n'))
          res$write(c('referer: ',req$referer(),'\n'))
          res$write(c('cookies: ',ls_str(req$cookies()),'\n'))
          res$write(c('content_charset: ',req$content_charset(),'\n'))
          res$write(c('head: ',req$head(),'\n'))
          res$write(c('accept_encoding: ',req$accept_encoding(),'\n'))
          res$write(c('content_length: ',req$content_length(),'\n'))
          res$write(c('form_data: ',req$form_data(),'\n'))
          res$write(c('xhr: ',req$xhr(),'\n'))
          res$write(c('params: ',ls_str(req$params()),'\n'))
          res$write(c('media_type_params:\n',ls_str(req$media_type_params()),'\n'))
          res$write(c('user_agent: ',req$user_agent(),'\n'))
          res$write(c('put: ',req$put(),'\n'))
          res$write(c('get: ',req$get(),'\n'))
          res$write(c('path: ',req$path(),'\n'))
          res$write(c('body: ',ls_str(req$body()),'\n'))
          res$write(c('port: ',req$port(),'\n'))
          res$write(c('host_with_port: ',req$host_with_port(),'\n'))
          res$write(c('scheme: ',req$scheme(),'\n'))
          res$write(c('ip: ',req$ip(),'\n'))
          res$write(c('options: ',req$options(),'\n'))
          res$write(c('to_url: ',req$to_url('foo',bar=1,baz='two'),'\n'))
          res$write(c('host: ',req$host(),'\n'))
          res$write(c('POST: ',ls_str(req$POST()),'\n'))
          res$write(c('trace: ',req$trace(),'\n'))
          res$write(c('script_name: ',req$script_name(),'\n'))
          res$write(c('content_type: ',req$content_type(),'\n'))
          res$write(c('delete: ',req$delete(),'\n'))
          res$write(c('path_info: ',req$path_info(),'\n'))
#          res$write(c('\nRac env: ',ls_str(as.list(env)),'\n'))
          res$finish()
        }
        )
  
  browseURL(sprintf("http://127.0.0.1:%s/custom/%s/indexgw.rhtml", tools:::httpdPort, app_name))
  
}

