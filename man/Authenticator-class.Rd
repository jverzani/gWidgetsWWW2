\name{Authenticator-class}
\alias{Authenticator-class}
\title{Base class for providing authentification}
\description{
  The \code{\link{load_app}} call allows one to specify a
  subclass of \code{Authenticator} to perform
  authentification before a user can access a main app.
  This subclass should provide at a minimum a method
  \code{is_valid_user} with arguments \code{user} and
  \code{pwd} to check for a valid user. The default
  implementation is wide open. The source code provides a
  simple example.
}

