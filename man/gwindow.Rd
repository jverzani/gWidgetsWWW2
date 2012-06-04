\name{gwindow}
\alias{gwindow}
\alias{GWindow}
\title{Main window constructor}
\usage{
  gwindow(title = "", parent = NULL, handler = NULL,
    action = NULL, ..., renderTo = NULL, width = NULL,
    height = NULL, ext.args = NULL)
}
\arguments{
  \item{title}{Window title}

  \item{parent}{One and only one gwindow per script should
  have no parent specified. Otherwise, this should be a
  \code{gwindow} instance.}

  \item{handler}{Handler called when window is closed. (For
  subwindows only)}

  \item{action}{action passed to handler}

  \item{...}{ignored}

  \item{renderTo}{Where to render window. For subwindows,
  this should be NULL. For main windows, this can be a DOM
  id or left as NULL, in which case the entire web page is
  used.}

  \item{width}{width of a subwindow in pixels.}

  \item{height}{height of a subwindow in pixels}

  \item{ext.args}{extra args passed to the constructor}
}
\value{
  An ExtContainer object
}
\description{
  There can be more than one gwindow instance per script,
  but one is special. This one is called without a
  \code{parent} object, which otherwise is typically
  another \code{gwindow} instance. The special window sets
  up the environment to store the callbacks etc. Subwindows
  are possible. Simply pass a value of \code{NULL} to the
  argument \code{renderTo}. This argument is used to
  specify the DOM id of a \code{DIV} tag. If given, the GUI
  created by the \code{gwindow} call will replace this part
  of the web page. If not given, then a subwindow will be
  rendered. % The \code{visible<-} method can be used to
  recompute the layout. This is often useful as the last
  line of a script.

  The \code{GWindow} class is used for windows and
  subwindows. Windows in \pkg{gWidgetsWWW2} are rendered to
  parts of the web page. In the simplest case, they are
  rendered to the document body and are the only thing the
  user sees. However, one can render to parts of a window
  as well. This is why we have a \code{renderTo} argument
  in the constructor.
}
\details{
  One of the instances on a page contains the "toplevel"
  object, which routes handler requests and gives web page
  responses.

  Subwindows are floating windows that appear on top of the
  web page, like a dialog box.

  The method \code{start_comet} will launch a long-poll
  process, whereby the browser repeatedly queries the
  server for any changes. This can be useful if one expects
  to launch a long-running process and the handler that
  initiates this will time out before the process is done.
  One needs only to add the javascript commands to the
  queue.
}
\examples{
w <- gwindow("Top level", renderTo="replaceme") ## no parent, so main one
g <- ggroup(cont=w)
b <- gbutton("click me for a subwindow", cont=g, handler=function(h,...) {
  w1 <- gwindow("subwindow -- no renderTo", renderTo=NULL, parent=w)
  g <- ggroup(cont=w1)
  gbutton("dispose", cont=g, handler=function(h,...) dispose(w1))
})
w2 <- gwindow("render elsewhere", parent=w, renderTo="replacemetoo") ## renderst to part of page
}

