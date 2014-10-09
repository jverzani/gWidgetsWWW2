##      Copyright (C) 2011  John Verzani
##  
##      This program is free software: you can redistribute it and/or modify
##      it under the terms of the GNU General Public License as published by
##      the Free Software Foundation, either version 3 of the License, or
##      (at your option) any later version.
##  
##      This program is distributed in the hope that it will be useful,
##      but WITHOUT ANY WARRANTY; without even the implied warranty of
##      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##      GNU General Public License for more details.
##  
##      You should have received a copy of the GNU General Public License
##      along with this program.  If not, see <http://www.gnu.org/licenses/>.

##' @include array.R
##' @include gwidgets-session.R
NULL

##' download extjs files and unzips into location to be found
##'
##' Useful if installing from a package which doesn't bundle. The
##' extjs files are quite large (nearly 70Mb download), so rather than
##' bundle in an R package, one can download as needed.
##' @return Nothing. Installs files if successful
##' @export
download_extjs <- function() {
    to <- paste(system.file("base", "javascript", package="gWidgetsWWW2"), "ext-4-2.1", sep=.Platform$file.sep)

    if (!file.exists(to)) {
        f <- "cdn.sencha.com/ext/gpl/ext-4.2.1-gpl.zip"
        tmp <- tempfile(fileext=".zip")
        download.file(f, tmp)
        unzip(tmp, exdir=to, junkpaths=TRUE)
    }
}


##' String class -- does not get escaped in object literals
##'
##' Useful in the toJSObject method
##' @param x character vector to coerce to String class
##' @return vector with new class addedd
##' @export
String <- function(x) {
  class(x) <- c("String", class(x))
  x
}

##' Internal function to call whisker.render and compose with String
##' @param tpl template
##' @param l data
##' @return a character containing filled in template
String_render <- function(tpl, l) {
  String(whisker.render(tpl, l))
}

## map R objects into object literals


##' Coerce an object to a string
##'
##' @param x object to coerce
##' @return returns object sometimes quoted
##' @export
coerceToJSString <- function(x) UseMethod("coerceToJSString")

##' S3 method for coerceToString
##' @method coerceToJSString default
##' @S3method coerceToJSString default
##' @rdname coerceToJSString
coerceToJSString.default <- function(x) x # no quote

##' S3 method for coerceToString
##' 
##' @method coerceToJSString character
##' @S3method coerceToJSString character
##' @rdname coerceToJSString
coerceToJSString.character <- function(x) sprintf("'%s'", escapeSingleQuote(x))

##' S3 method for coerceToString
##'
##' @method coerceToJSString factor
##' @S3method coerceToJSString factor
##' @rdname coerceToJSString
coerceToJSString.factor <- function(x) coerceToJSString(as.character(x))

##' S3 method for coerceToString
##' 
##' @method coerceToJSString logical
##' @S3method coerceToJSString logical
##' @rdname coerceToJSString
coerceToJSString.logical <- function(x) tolower(as.character(x))

##' S3 method for coerceToString
##' 
##' @method coerceToJSString String
##' @S3method coerceToJSString String
##' @rdname coerceToJSString
coerceToJSString.String <- function(x) x # to avoid quoting

##' S3 method for coerceToString
##' 
##' @method coerceToJSString list
##' @S3method coerceToJSString list
##' @rdname coerceToJSString
coerceToJSString.list <- function(x) toJSObject(x)

##' map an R list object into a string containing javascript code representation of an object
##'
##' @param x a list that will map to an object literal. E.g., list(a=1, b="a") goes to {a:1, b:'a'}
##' @return a string with the list formatted as code to produce a JavaScript object
##' @export
##' @examples
##' arg_list <- list(
##'   tooltip = "some tooltip",
##'   width = 200,
##'   height = "auto",
##'   id = String("noquote")
##' )
##' toJSObject(arg_list)
##' 
toJSObject <- function(x) {
  out <- c()

  for(i in names(x)) {
    val <- x[[i]]
    if(is.null(val) || (length(val) == 0) || (length(val) == 1) && is.na(val)) {
      ## nada
    } else if(is.list(val)) {
      out[i] <- toJSObject(val)
    } else {
      ## print(list("toJSObject", length1=coerceToJSString(val)))
      out[i] <- coerceToJSString(val)
    }
  }
  nms <- names(out); ind <- grepl("-", nms) # a-b-c -> "a-b-c"
  nms[ind] <- sprintf('"%s"', nms[ind])
  sprintf("{%s}", paste(paste("'",nms,"'", sep=""), out, sep=":", collapse=","))
}

## ## coerce a single value to javascript with quotes
## ## logical is buggy
## toJS <- function(x) UseMethod("toJS")
## toJS.default <- function(x) shQuoteEsc(x)
## toJS.logical <- function(x) tolower(as.character(x))
## toJS.integer <- toJS.numeric <- function(x) x
## toJS.factor <- function(x) toJS(as.character(x))


##' An empty array
##'
##' Used by toJSArray as helper function
##' @param doBrackets logical
##' @export
emptyJSArray <- function(doBrackets=TRUE)  ifelse(doBrackets, "[]", "")

##' Make a JS array from an R object
##'
##' @param x R object to make into an array
##' @param doBrackets logical Use brackets in ouput []
##' @return JSON encoded value from arrary
##' @export
toJSArray <- function(x, doBrackets=TRUE) UseMethod("toJSArray")

##' ToJSArray method
##' 
##' @method toJSArray default
##' @S3method toJSArray default
##' @rdname toJSArray
toJSArray.default <- function(x, doBrackets=TRUE) stop("no default method")

##' ToJSArray method
##' 
##' @method toJSArray numeric
##' @S3method toJSArray numeric
##' @rdname toJSArray
toJSArray.numeric <- function(x, doBrackets=TRUE) {
  if(!length(x)) return(emptyJSArray(doBrackets))
  x <- as.character(x)
  x[is.na(x)] <- "'NA'"
  out <- paste(x, collapse=",")
  if(doBrackets)
    out <- paste("[",out,"]", sep="")
  return(out)
}

##' ToJSArray method
##'  
##' @method toJSArray String
##' @S3method toJSArray String
##' @rdname toJSArray
toJSArray.String <- function(x, doBrackets=TRUE) {
  if(!length(x)) return(emptyJSArray(doBrackets))  
  x <- gsub("\\n", " ", x)              # \n messes up JS parsing
  out <- paste(x, collapse=",")
  if(doBrackets) out <- paste("[", out,"]",sep="")
  return(out)
}

##' ToJSArray method
##' 
##' @method toJSArray logical
##' @S3method toJSArray logical
##' @rdname toJSArray
toJSArray.logical <- function(x,doBrackets=TRUE) {
  if(!length(x)) return(emptyJSArray(doBrackets))
  x <- tolower(as.character(x))
  x[is.na(x)] <- "'NA'"
  toJSArray.String(x, doBrackets)
}

##' ToJSArray method
##' 
##' @method toJSArray character
##' @S3method toJSArray character
##' @rdname toJSArray
toJSArray.character <- function(x, doBrackets=TRUE) {
  if(!length(x)) return(emptyJSArray(doBrackets))  
  x <- sprintf("'%s'", escapeSingleQuote(x))
  toJSArray.String(x, doBrackets)
}

##' ToJSArray method
##' 
##' @method toJSArray factor
##' @S3method toJSArray factor
##' @rdname toJSArray
toJSArray.factor <- toJSArray.character

##' ToJSArray method
##' 
##' @method toJSArray Date
##' @S3method toJSArray Date
##' @rdname toJSArray
toJSArray.Date <- function(x, doBrackets=TRUE) {
  ## SHould make format an option. Here it needs to match \code{column_xtype.Date}.
  toJSArray.String(ourQuote(format(x, "%Y/%m/%d")), doBrackets)
}

##' ToJSArray method
##' 
##' @method toJSArray matrix
##' @S3method toJSArray matrix
##' @rdname toJSArray
toJSArray.matrix <- function(x, doBrackets=TRUE) {
  out <- paste(apply(x,1,toJSArray), collapse=",")
  if(doBrackets) out <- paste("[", out, "]", sep="")
  return(out)
}


##' ToJSArray method  
##' 
##' @method toJSArray list
##' @S3method toJSArray list
##' @rdname toJSArray
toJSArray.list <- function(x, doBrackets=TRUE) {
  sapply(x, function(i) toJSArray(i,doBrackets))
}
       
## This needs work
##' ToJSArray method
##' 
##' @method toJSArray data.frame
##' @S3method toJSArray data.frame
##' @rdname toJSArray
toJSArray.data.frame <- function(x,doBrackets=TRUE) {
  if(nrow(x) == 0) {
    n <- ncol(x)
    out <- paste(rep("[]", n), collapse=",")
    if(doBrackets)
      out <- sprintf("[%s]", out)
    return(out)
  }
  ## depends on number of cols
  if(ncol(x) == 1)
    return(toJSArray(x[,1,drop=TRUE]))

  ## otherwise, we need to work
  tmp <- sapply(x, function(y) toJSArray.list(y, doBrackets=FALSE))
  if(!is.matrix(tmp))
    tmp <- matrix(tmp, ncol=length(tmp))

  tmp1 <- apply(tmp,1,function(i) paste("[",paste(i,collapse=","),"]",sep=""))
  out <- paste(tmp1, collapse=",")
  if(doBrackets) out <- paste("[",out,"]",sep="")
  return(out)
}


escape_html <- function(string) Rook::Utils$escape_html(string)
unescape_html <- function(string) {
  if (is.null(string)) base::stop("Need a character vector argument")
  string <- gsub('&amp;','&',string)
  string <- gsub('&lt;','<',string)
  string <- gsub('&gt;','>',string)
  string <- gsub('&#39;',"'",string)
  string <- gsub('&quot;','"',string)
  string
}



##' escaping strings
##' 
##' we use shQuote as a convenience for
##' word -> 'word' however, it doesn't escape values as we would like, hence
##' this one.
##' @param x character
##' @return character has single quotes escaped
shQuoteEsc <- function(x) {
  out <- gsub("\'","\\\\'",x)
  out <- paste("'",out,"'",sep="")
  return(out)
}
 
##' make a string safe to pass in as HTML fragment.
##'
##' We pass in strings as 'string contents' so we replace all ' quotes with " ones, then wrap in ''
##' @param x a string to replace ' with
##' @return a string with quotes escaped and placed within ''
ourQuote <- function(x) {
  x <- gsub("'",'"',x)
  x <- gsub("\n", "\\\\n", x)           # \n's are issue
  sprintf("'%s'", x)
}


##' escape single quote
##'
##' Useful as we often single quote arguments to the Ext functions
##' @param x string to escape
##' @return string with "'" replaced by "\'"
escapeSingleQuote <- function(x) {
  ## This coudl be *much* improved if only I knew regular expressions better
  esc_q <- "[\\][']"
  stupid_match <- "THISWOULDNEVERHAPPEN"
  x <- gsub(esc_q, stupid_match,x)
  x <- gsub("'", "\\\\'", x)
  x <- gsub(stupid_match, "\\\\'", x)
  x <- gsub("\n", "\\\\n", x) 
  ## gsub("'", '"', x)  ## could replace, but escaping seems safer
  #gsub("\\'", "\\\\'", x)
  return(x)
}


##' get value from ... arguments
##' @param key key to lookip
##' @param ... passed from function
##' @param default default value if NULL
getFromDots <- function(key, ..., default=NULL) {
  val <- list(...)[[key]]
  if(is.null(val) && !is.null(default))
    default
  else
    val
}

##' merge two lists
##' 
##' @param x a list
##' @param y a list
##' @param overwrite logical should we overright values in x
merge.list <- function(x, y, overwrite=TRUE) {
  if(missing(y) || is.null(y))
    return(x)
  for(i in names(y))
    if((is.logical(overwrite) && overwrite) || !(i %in% names(x)))
      x[[i]] <- y[[i]]
  x
}

##

##' Is value a URL: either of our class URL or matches url string: ftp://, http:// or file:///
##'
##' @param x length 1 character value to test
##' @return Logical indicating if a URL.
##' @export
isURL <- function(x) {

  ## we can bypass this by setting a value to have this class
  ## as in isURL((class(x) <- "URL"))
  if(is(x,"URL")) return(TRUE)
  if (is.character(x) && length(x) == 1) 
    out <- length(grep("^(ftp|http|file)://", x)) > 0
 else
   out <- FALSE
  return(out)
}

##' Add URL to class of object if not already
##'
##' @param x object to add class to. Should be length 1 character
##' @return returns object
##' @export
asURL <- function(x) {
  if(!is(x,"URL"))
    class(x) <- c("URL",class(x))
  return(x)
}


##' Get a temporary file matching the given extension, if given.
##'
##' This tempfile is stored in a directory that can be served as a static file through
##' gWidgetsWWW2.
##' @param ext file extension
##' @export
##' @examples
##' f <- get_tempfile(ext=".svg")
##' get_tempfile_url(f)
get_tempfile <- function(ext=".txt") {
  f <- tempfile(pattern="gWidgetsWWW", tmpdir="", fileext=ext)

  if(!is.null(getOption("gWidgetsWWW2:FastRWeb"))) {
    f <- tempfile(pattern="gWidgetsWWW", tmpdir="/var/FastRWeb/tmp/", fileext=ext)
#    f <- sprintf("/var/FastRWeb/tmp/%s", f)
  } else {
    dir.create(sprintf("%s%s%s",tempdir(), .Platform$file.sep, "tmp"), showWarnings=FALSE)
    f <- sprintf("%s/tmp%s", tempdir(), f)
  }
  class(f) <- c("StaticTempFile", class(f))
  f
}

##' Get url for a tempfile created by \code{get_tempfile}
##'
##' Requires the tempdir to be mapped to a specific url
##' @param f a file name produced by \code{get_tempfile}
##' @export
##' @examples
##' f <- get_tempfile(ext=".svg")
##' get_tempfile_url(f)
get_tempfile_url <- function(f) {
  if(!is(f, "StaticTempFile"))
    return(f)
  ## This should be configurable! (The first tmp anyways)
  if(!is.null(getOption("gWidgetsWWW2:FastRWeb")))
    asURL(sprintf("/cgi-bin/R/tmp?file=%s", basename(f)))
  else
    asURL(sprintf("/custom/tmp/tmp/%s", basename(f)))
}

DEBUG <- function(...) print(list(...))
logger <- function(..., fname="/tmp/log.txt") cat(..., "\n", file=fname, append=TRUE)
  
##' Is the process running locally?
##'
##' @export
##' @return logical value, \code{TRUE} if local, \code{FALSE} if not.
is_running_local <- function() {
  is.null(getOption("gWidgetsWWW2:FastRWeb"))
}

##' Make a template for the rapache configuration files
##'
##' To use rapache a few locations need to be defined so that ajax
##' calls and calls to get local files are recognized. This happens in
##' a configuration file and this function makes a template that will
##' work, yet allows ucustomization.
##' @param file passed to \code{cat} function to control what happens to output
##' @note The resulting file must be placed into the apached directory (/etc/apache2?)
##' and integrated into the configuration system.
##' @export
make_rapache_files <- function(file="") {
  tpl <- system.file("rapache", "apache.conf", package="gWidgetsWWW2")
  gWidgetsWWW2_home <- system.file("rapache", package="gWidgetsWWW2")
  cat(whisker.render(paste(readLines(tpl), collapse="\n"),
                     list(gWidgetsWWW2_home=gWidgetsWWW2_home)), file=file)
}

##' Make an apache configuration for a given app
##'
##' An rapache gWidgetsWWW2 app needs to integrate within the Rook
##' system. The template produced by \code{make_rapache_files} shows
##' how to configure the apache server for an app, but needs to call a
##' script \code{loadapp.R}. This function produces such a script.
##' @param file  passed to \code{cat} function to control what happens to output
##' @note this file is placed into a directory specified in the rapache configuration
##' @export
make_rapache_loadapp <- function(file="") {
  tpl <- system.file("rapache", "loadapp.R", package="gWidgetsWWW2")
  cat(whisker.render(paste(readLines(tpl), collapse="\n")),
       file=file)
}



## Singleton pattern
Singleton <- setRefClass("Singleton",
                             fields=list(
                               Class="ANY",
                               instance="ANY"
                               ),
                            methods=list(
                              initialize=function(...) {
                                "Override this by defining Class"
                                instance <<- NULL
                                 callSuper(...)
                               },
                               get_instance=function(...) {
                                 "Get a unique instance of the class defined in the subclasses initialize method"
                                 if(is.null(instance)) 
                                   instance <<- Class$new(...)
                                 instance
                               }
                               ))
