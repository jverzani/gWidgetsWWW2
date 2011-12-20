\name{GCanvas}
\alias{GCanvas}
\title{class for gcanvas}
\description{
  The gcanvas widget provides a canvas for drawing on or
  for displaying graphics produced by the canvas device.
  The drawing is supported through a series of low-level
  primitives: \code{polygon}, \code{lines}, \code{rect},
  \code{circle}. Others are possible. These are R5 methods
  of the canvas object. In addition, there are additional
  handlers: \code{add_handler_clicked},
  \code{add_handler_mouse_motion}, .... These are passed
  back event information through the \code{h} object. The
  method \code{get_event_params} provides the mapping.
}

