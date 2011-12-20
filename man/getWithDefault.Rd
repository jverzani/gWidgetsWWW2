\name{getWithDefault}
\alias{getWithDefault}
\title{Get an object using default if otherwise not defined}
\usage{
  getWithDefault(x, default)

  getWithDefault(x, default)
}
\arguments{
  \item{val}{value to get}

  \item{default}{default to use if null, NA or 0 length}

  \item{x}{value to return or its default}

  \item{default}{default value}
}
\value{
  x or default
}
\description{
  if val is null etc, return default, else val

  Return x unless NULL, NA, length 0, ..., in which case we
  give default
}

