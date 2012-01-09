\docType{package}
\name{gWidgetsWWW2-package}
\alias{GCanvas}
\alias{GComponent}
\alias{GWidget}
\alias{gWidgetsWWW2-package}
\alias{GWindow}
\title{An implementation of the gWidgets API for web programming}
\description{
  This package uses the \pkg{Rook} package of Jeffrey
  Horner to allow R programmers to easily create
  interactive web pages from within an R session.

  The basic setup of gWidgetsWWW2 involves a set of
  constructors and S3 methods for manipulating the
  constructed objects. The constructors return a reference
  class object. The S3 methods then simply pass arguments
  along to the appropriate reference class method. Of
  course, these may be called directly, but for portability
  of gWidgets code to other toolkit this is not
  recommended. However, for methods that are only
  implemented in a given toolkit, such reference method
  calls becomes necessary and desirable. In documenting the
  reference class objects just those exposed methods which
  must be called as reference methods are mentioned.

  GWidget is the Base class for widget objects. See
  \code{gcontainer} for the base class for container
  objects. Both derive from the \code{GComponent} class.

  The \code{GWindow} class is used for windows and
  subwindows. Windows in \pkg{gWidgetsWWW2} are rendered to
  parts of the web page. In the simplest case, they are
  rendered to the document body and are the only thing the
  user sees. However, one can render to parts of a window
  as well. This is why we have a \code{renderTo} argument
  in the constructor.

  The \code{gcanvas} widget provides a canvas for drawing
  on or for displaying graphics produced by the
  \pkg{canvas} device. The drawing is supported through a
  series of low-level primitives: \code{polygon},
  \code{lines}, \code{rect}, \code{circle}. Others are
  possible. These are reference class methods of the
  gcanvas object. In addition, there are additional
  handlers: \code{add_handler_clicked},
  \code{add_handler_mouse_motion}, .... These are passed
  back event information through the \code{h} object. The
  method \code{get_event_params} provides the mapping.
}
\details{
  The basic interface is meant to be implemented by all
  gWidgets implementations. The forthcoming gWidgets2 will
  expect that. So gWidgets2WWW will use the basic
  interface, but for now gWidgetsWWW2 does only for the
  most part.

  The GComponent class is used to define methods common to
  most all the widgets. This class also includes methods
  for processing callbacks. This is different in
  gWidgetsWWW2. The basic idea is that JavaScript is used
  to make a callback into the session containing a session
  id (to find the correct evaluation environment), a object
  id (to find the signaling object), a signal (to look up
  the handlers assigned to that object for the given
  signal) and possibly some extra parameters. The latter
  are there to bypass the transport calls that are used to
  synchronize the widget state from the browser with the R
  session data. These calls are asynchronous so may not
  have been processed when the handler call is processed.

  The lookup used above requires each widget to be
  registered in a toplevel object which is unique to a
  page. This toplevel object is found from the session id,
  then looks up the object from the passed in object id.
  This toplevel object is passed into a widget via either
  the \code{container} argument or the \code{parent}
  argument. In addition to routing requests to handlers,
  the toplevel object also is used to send back JavaScript
  commands to the browser. The method \code{add_js_queue}
  is all that is needed for this. A convenience method
  \code{call_Ext} provides an alternative. For this method
  one specifies a method name and named arguments that are
  converted to a JavaScript object to parameterize the
  method call. Somewhat reverse to this is calling an R
  object from JavaScript. The method \code{call_rpc} is
  used for this, where in the JavaScript one uses
  \code{jRpc} to initiate the call and
  \code{add_public_method} to register that an object's
  method is available to be called in this manner.

  Methods related to the handler code are
  \code{add_handler}, \code{invoke_handler},
  \code{handler_widget}, \code{connect_to_toolkit},
  \code{transport_fun}, \code{process_transport},
  \code{param_defn}, \code{before_handler}

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
\author{
  John Verzani \email{jverzani@gmail.com}
}
\keyword{package}

