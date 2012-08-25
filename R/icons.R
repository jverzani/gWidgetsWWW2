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

##' @include gwidget.R
NA

##' Mark a character as an Icon
##'
##' We use the class "Icon" to mark icons, which are css classnames or URLs
##' @param x a character vector to mark with additional class of "Icon"
##' @return same object with new class
##' @export
asIcon <- function(x) {
  class(x) <- c("Icon", class(x))
  x
}

##' Is this an icon? (has class Icon)
##' @param x object
##' @return logical
##' @export
isIcon <- function(x) is(x,"Icon")


## basename without extension
## @param x file to extract basename from
.our_basename <- function(x) {
  b <- basename(x)
  if(grepl("\\.", b)) {
    x <- strsplit(b, "\\.")[[1]]
    b <- paste(x[-length(x)], collapse=".")
  }
  b
}

## make an icon class helper
## @param f file
## @param url of icon
## @param prefix prefix of icon class name
.make_icon_class <- function(f, url=f, prefix="x-gw-icon") {
## f is filename, url is url of file
  b <- .our_basename(f)
  list(id=asIcon(sprintf("%s-%s", prefix, b)),
       url = url,
       css = sprintf("{background-image:url(%s) !important; background-repeat:no-repeat;background-position:center;}", url)
       )
}

## internal array of stock icons
.gWidgets_stock_icons <- Array$new()         # list of stock icons classes and urls

## populate stock icons array. Return icons for inclusion into style sheet
##' make stock icons
##'
##' @export
make_stock_icons <- function() {
  fs <- list.files(system.file("base/images", package="gWidgetsWWW2"), full.names=TRUE)
  bs <- basename(fs)
  bs_noexts <- sapply(fs, .our_basename)
  if(!is.null(getOption("gWidgetsWWW2:FastRWeb")))
    urls <- sprintf("/cgi-bin/R/gWidgetsWWW2?name=images/%s", bs)
  else
    urls <- sprintf("/custom/gWidgetsWWW2/images/%s", bs)
  css <- Array$new(mapply(.make_icon_class, fs, urls, SIMPLIFY=FALSE))

  css$each(function(i, key, value) {
    .gWidgets_stock_icons$push(value, bs_noexts[i])
  })

  ## return, but don't show
  invisible(sprintf(".%s %s", css$pluck("id"), css$pluck("css")))
}


##' Return stock icons
##'
##' These appear as a list with a name, CSS class (setIconClass), and url (setIcon)
##' @return a list of icons. Unlike other gWidgets
##' implementations. Each component is alist with css name and url, as
##' both are useful.
##' @export
getStockIcons <- function() {
  if(.gWidgets_stock_icons$len() == 0)
    make_stock_icons()
  
  .gWidgets_stock_icons$core()
}

##' Get the icon class (or url) by its name
##'
##' @param icon_name icon name to be looked up in stock icons
##' @param css logical. If TRUE, return css class name, else url
##' @return Icon css class (or url) or NA if not there
##' @export
##' @examples
##' getStockIconByName("up") ## "x-gw-icon-up"
##' getStockIconByName("Not there") ## NULL
getStockIconByName <- function(icon_name, css=TRUE) {
  if(missing(icon_name) || is.null(icon_name))
    return(NULL)
  l <- getStockIcons()
  i <- l[[as.character(icon_name), exact=TRUE]]
  if(is.null(i))
    return(NULL)
  
  asIcon(ifelse(css, i$id, i$url))
}

##' Add to the stock icons
##'
##' A stock icon requires a name and url. The name can be used to look
##' up the css class used by the widgets using the function
##' \code{\link{getStockIconByName}}.
##' @param iconNames character vector of names. May be data frame with first two columns being names, then urls
##' @param iconFiles character vector of icon urls (not files, despite the name of the function)
##' @param ... ignored
##' @param parent a \code{gwindow} instance. Needed with gWidgetsWWW2.
##' @return Modifies internal stock icon list, but has no useful return value
##' @export
##' @examples
##' w <- gwindow()
##' addStockIcons("rlogo", "http://www.r-project.org/Rlogo.jpg", parent=w) ## add one
##' getStockIconByName("rlogo")  ## check
##' @note Although this function can be called more than once, some browsers will balk if called more than 31 times.
addStockIcons <- function(iconNames, iconFiles, ..., parent) {
  if(is.data.frame(iconNames)) {
    iconFiles <- iconNames[,2]
    iconNames <- iconNames[,1]
  }
  css <- Array$new(mapply(.make_icon_class, iconNames, iconFiles, SIMPLIFY=FALSE))
  
  css$each(function(i, key, value) {
    .gWidgets_stock_icons$push(value, iconNames[i])
  })
  
  css_txt <- sprintf(".%s %s", css$pluck("id"), css$pluck("css"))
  cmd <- sprintf("Ext.util.CSS.createStyleSheet(%s);",
                 ourQuote(paste(css_txt, collapse=" ")))
  parent$add_js_queue(cmd)
}



