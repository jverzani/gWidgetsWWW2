% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/utils.R
\name{toJSObject}
\alias{toJSObject}
\title{map an R list object into a string containing javascript code representation of an object}
\usage{
toJSObject(x)
}
\arguments{
\item{x}{a list that will map to an object literal. E.g., list(a=1, b="a") goes to {a:1, b:'a'}}
}
\value{
a string with the list formatted as code to produce a JavaScript object
}
\description{
map an R list object into a string containing javascript code representation of an object
}
\examples{
arg_list <- list(
  tooltip = "some tooltip",
  width = 200,
  height = "auto",
  id = String("noquote")
)
toJSObject(arg_list)
}

