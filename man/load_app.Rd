\name{load_app}
\alias{load_app}
\title{Load a web app defined by a gWidgetsWWW2 script}
\usage{
  load_app(script_name, app_name = gsub("\\..*", "",
  basename(script_name)), port = 9000L, session_manager =
  make_session_manager(), open_page = TRUE, brew_template =
  "", show.log = FALSE, authenticator = NULL, ...)
}
\arguments{
  \item{script_name}{path to gWidgetssWWW2 script}

  \item{app_name}{base name for script, unique per project}

  \item{brew_template}{The script may render to the entire
  page, or parts of the page specified by an ID. The allows
  one to specify a brew template to hold these place
  holders and other HTML code.}

  \item{show.log}{If TRUE, logged information is written to
  the console}

  \item{port}{Initial port for Rhttpd server, provided it
  hasn't already started.}

  \item{authenticator}{Used to specify a subclass of
  \code{\link{Authenticator-class}} for doing
  authentification. Specifying a value of \code{TRUE} will
  do a trivial authentification. Subclasses of
  \code{Authenticator} should implement the methods
  \code{is_valid_cookie} and \code{is_valid_user}.}

  \item{session_manager}{an instance of
  \code{make_session_manager}}

  \item{open_page}{logical. If \code{TRUE} call
  \code{browseURL} to open the app.}

  \item{...}{passed to \code{brew} call of
  \code{brew_template}}
}
\description{
  There are two basic tasks that gWidgetsWWW2 does: a)
  create the javascript to populate a page (or part of the
  page) and b) create an means for AJAX requests between
  the web browser and the R process. For b) there isn't
  much to say except that it is supposed to just work.
}
\details{
  As for a) there are things one can pay attention to. The
  simplest case is really pretty simple. Write some script,
  save it to a file and call this function with the script
  name (and path if needed). This simple case will create a
  full screen app. Each app runs in its own environment in
  the R session, so running lots of instances can be
  expensive. That is, don't expect gWidgetsWWW2 to scale.

  There are other cases one might want. An app can run
  within a web page. The web template can be passed through
  the \code{brew_template} argument with the \code{...}
  parameters passed along.

  One might even be able to copy the provided indexgw.rhtml
  file and modify that. When using either of these
  approaches, pass a DOM id to the \code{renderTo} argument
  of the \code{gwindow} instance.

  Apps can require authentification. By default, this is
  not validated. One can write a subclass of
  \code{Authenticator} to do other validations. (Of course
  this needs improvement).
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

