\name{insert}
\alias{insert}
\title{Insert text into \code{gtext}}
\usage{
  insert(obj, value, where = "end", font.attr = NULL,
  do.newline = TRUE, ...)
}
\arguments{
  \item{obj}{object}

  \item{value}{text value}

  \item{where}{where to insert \code{c("end", "beginning",
  "at.cursor")}}

  \item{font.attr}{ignored. Font attribute for new text}

  \item{do.newline}{Do we add a new line?}

  \item{...}{ignored}
}
\value{
  no useful return value
}
\description{
  Used to insert text into \code{gtext} insances
}

