\name{load_app}
\alias{load_app}
\title{load a web app defined by a gWidgetsWWW2 script}
\usage{
  load_app(script_name, app_name = "test", brew_template =
  "", use.googlemap = FALSE, show.log = FALSE, port = 9000,
  ...)
}
\arguments{
  \item{script_name}{path to gWidgetssWWW2 script}

  \item{app_name}{base name for script, unique per project}

  \item{brew_template}{The script may render to the entire
  page, or parts of the page specified by an ID. The allows
  one to specify a brew template to hold these place
  holders and other HTML code.}

  \item{use.googlemap}{If using \code{ggooglemaps} include
  this so that the JavaScript files are downloaded.}

  \item{show.log}{If TRUE, logged information is written to
  the console}

  \item{port}{Initial port for Rhttpd server, provided it
  hasn't already started.}

  \item{...}{passed to \code{brew} call of
  \code{brew_template}}
}
\description{
  load a web app defined by a gWidgetsWWW2 script
}
\examples{
## open an app that takes the entire page
gw_script <-  system.file("examples/hello-world.R", package="gWidgetsWWW2")
if(interactive()) load_app(gw_script, "HelloApp",  use.google=FALSE)
## open an app using googlemaps
if(interactive()) load_app(system.file("examples/ggooglemaps.R", package="gWidgetsWWW2"),  "GoogleMapApp", use.google=TRUE)
## open an app embedded in another page
gw_script <- system.file("examples/ex-multiple-gwindow.R", package="gWidgetsWWW2")
brew_template <- system.file("framework/brew/custom.rhtml", package="gWidgetsWWW2")
if(interactive()) load_app(gw_script, "MultipleApp",  brew_template)
}

