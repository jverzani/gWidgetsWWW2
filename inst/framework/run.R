#require(devtools)
#load_all("gWidgetsWWW2")
if(0) {
  require(Rack)
require(brew)
require(RJSONIO)

require(canvas)




source("../../R/array.R")
source("../../R/utils.R")
source("../../R/icons.R")
source("../../R/brewery.R")
source("../../R/json-rpc.R")

source("../../R/gwidgets-toplevel.R")
source("../../R/gwidgets-session.R")
source("../../R/gwidgets-app.R")

source("../../R/ext-base.R")

source("../../R/ext-component.R")
source("../../R/ext-widget-proxy.R")

source("../../R/ext-container.R")
source("../../R/ext-widget.R")
source("../../R/ext-widget-text.R")

source("../../R/gaction.R")
source("../../R/gwindow.R")
source("../../R/ggroup.R")
source("../../R/gframe.R")
source("../../R/gexpandgroup.R")
source("../../R/gnotebook.R")
source("../../R/gpanedgroup.R")
source("../../R/gbutton.R")

source("../../R/gcanvas.R")
source("../../R/gcheckboxgroup.R")
source("../../R/gcombobox.R")
source("../../R/gedit.R")
source("../../R/gtext.R")
source("~/pmg/gw2/gprocessingjs.R")
source("../../R/glabel.R")
source("../../R/gradio.R")
source("../../R/gdialogs.R")
source("../../R/gslider.R")
source("../../R/gspinbutton.R")
source("../../R/gstatusbar.R")
source("../../R/gcalendar.R")
source("../../R/gtable.R")
source("../../R/gmenu.R")
source("../../R/ghtml.R")
source("../../R/gimage.R")
source("../../R/gfile.R")
source("../../R/glayout.R")
source("../../R/gsvg.R")
source("../../R/gtree.R")
source("../../R/gdf.R")
source("../../R/gwidgets-methods.R")
source("../../R/ggooglemaps.R")
## Write this as  a function!
## an app
}

open_project <- function(app_name,
                         script_name,
                         brew_template = NULL,
                         use.googlemap = TRUE,
                         debug=TRUE
                         ) {

  options("Rhttpd_debug"=as.logical(debug))

  
  R <- Rhttpd$new()
  try(R$start(), silent=TRUE)

  ## gWidgetsWWW, static files
  gWidgetsWWW <- Rack::Static$new(
                                  urls = c("/images", "/javascript", "/css"),
                                  root = system.file("base", package="gWidgetsWWW2")
                                  )
  R$add(RhttpdApp$new(gWidgetsWWW, name="gWidgetsWWW2"))
  
  ## tmpdir
  tmpApp <- Rack::Static$new(
                             urls=c("/tmp"),
                             root=tempdir()
                             )
  R$add(RhttpdApp$new(tmpApp, name="tmp"))
  
  ## brew index
  brewery <- Rack:::Brewery$new(url="/",
                                root=system.file("framework/brew", package="gWidgetsWWW2"),
                                app_name = app_name,
                                brew_template=brew_template,
                                use.googlemap = use.googlemap
                                )
  R$add(RhttpdApp$new(brewery, name="gwbrew"))
  
  ## an application
  app <- Rack::Builder$new(
                           ## app specific static files
                           Rack:::Static$new(
                                             urls = c('/css','/images','/javascript'), 
                                             root = '.'
                                             ),
                           ## brew files
                           Rack:::Brewery$new(
                                              url=sprintf("%s/brew", app_name),
                                              root='.'
                                              ),
                           ## why do I need this?
                           tmpApp,
                           gWidgetsWWW2:::GWidgetsApp$new(url="/gwapp", script=script_name),
                           ## why does this fail?
                           Rack:::Redirect$new(sprintf('%s/brew/indexgw.rhtml', app_name))
                           )
  
  R$add(RhttpdApp$new(app, name=app_name))

  browseURL(sprintf("http://127.0.0.1:%s/custom/gwbrew/indexgw.rhtml", tools:::httpdPort, app_name))
  
}
library(gWidgetsWWW2)
#open_project("MyApp", "gw-script.R", "custom.rhtml")
open_project("NewApp", "/tmp/script.R", "/tmp/extra.html", use.google=TRUE)
