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
NULL

##' gtree widget
##'
##' Widget to display heirarchical data. The data is described by a
##' function which returns the offspring for agiven node in a specific
##' format. This implementation can only show one column's worth of
##' data and an icon.
##' 
##' @param offspring function with signature (path, data). Returns a
##' data frame with columns: key (which creates the path), offspring
##' (a logical indicating if the node has children), icon (a character
##' vector of stock icons names or ""), and any other values to
##' display in subsequent columns. The column names are mapped to
##' header names. A column named \code{qtip} will be rendered as a
##' tooltip, not as another column. The order of the columns is used,
##' so make sure it goes key, offspring, icon, any other
##'
##' @param offspring.data passed to offspring call so that the
##' offspring function can be parameterized if desired.
##' 
##' @param multiple logical do we allow multiple selection. NOT IMPLEMENTED
##'
##' @inheritParams gwidget
##' @note  TODO: implement multiple slection
##' @export
##' @examples
##' # galton watson
##' \dontrun{
##' offspring <- function(path, ...) {
##'   x <- rbinom(2, 1, p=1/2)
##'   icons <- c("dismiss","ok")[2-x]
##'   nms <- c("branch","leaf")[x+1]
##'   ttip <- paste("This is a ", nms)
##'   data.frame(id=letters[1:2], hasoffspring=as.logical(x), icons=icons,
##'               value=nms, qtip=ttip, stringsAsFactors=FALSE)
##' }
##' w <- gwindow("Galton Watson tree")
##' g <- ggroup(cont=w, horizontal=FALSE)
##' ghtml("A node in a Galton-Watson tree has 0 or 2 offspring.<br />
##'        In this case each has equal chance.", cont=g)
##' gseparator(cont=g)
##' tr <- gtree(offspring=offspring,  cont=g)
##' size(tr) <- c(300,300)
##' b <- gbutton("Try again", cont=g, handler=function(h,...) tr$update())
##' visible(w) <- TRUE
##' }
gtree <- function(offspring = NULL,
                  offspring.data = NULL,
                  multiple = FALSE, 
                  handler = NULL, action = NULL,
                  container = NULL,
                  ...,
                  width=NULL,
                  height=NULL,
                  ext.args=NULL
                  ) {

  tr <- GTree$new(container)
  tr$init(offspring, offspring.data, 
          multiple, handler, action, container, ...,
          width=width, height=height, ext.args=ext.args)
  tr
}

## base class for gtree
GTree <- setRefClass("GTree",
                     contains="GWidget",
                     fields=list(
                       store="ANY",
                       path = "character",
                       cur_record="ANY"
                       ),
                     methods=list(
                       init=function(offspring, offspring.data, 
                         multiple, handler, action, container, ...,
                         width=NULL, height=NULL, ext.args=NULL) {

                         initFields(path=character(0),
                                    cur_record=list(),
                                    constructor="Ext.tree.Panel",
                                    transport_signal="selectionchange",
                                    change_signal="selectionchange"
                                    )
                         
                         ## create proxy
                         store <<- GWidgetTreeStore$new(container)
                         store$init(container, offspring, offspring.data)


                         ## arg_list
                         arg_list <- list(
                                          collapsible=TRUE,
                                          useArrows=TRUE,
                                          trackMouseOver=TRUE,
                                          rootVisible=FALSE,
                                          #autoScroll=TRUE,
                                          animate=TRUE,
                                          preventHeader=TRUE,
#                                          border=FALSE,
#                                          hideBorders=TRUE,
                                          enableDrag=TRUE,
                                          width=width,
                                          height=height,
                                          store=String(store$get_id()),
                                          columns=store$proxy$make_columns()
                                          )
                         add_args(arg_list)

                         setup(container, handler, action, ext.args, ...)

                         ## nodeToPath
                         cmd <- paste("nodeToPath = function(n) {",
                                      "var origNode=n;",
                                      "var a = new Array();",
                                      "var ids = new Array();",
                                      "var p = n.parentNode;",
                                      "while (p) {",
                                      "  a.push(p.indexOf(n));",
                                      "  ids.push(n.id);",
                                      "  n = p;",
                                      "  p = n.parentNode",
                                      "};",
                                      "var param={value:{index: a.reverse(), ids: ids.reverse().join(':.:'), record:origNode.raw}};",
                                      "return param;",
                                      "};",
                                      sep="")
                         add_js_queue(cmd)
                         
                         ## ## beforeload puts in id and session id
                         ## cmd <- paste(sprintf("%s.getLoader().on('beforeload', function(loader, node) {", get_id()),
                         ##              sprintf("loader.baseParams.id = %s;", ourQuote(proxy$get_id())),
                         ##              "loader.baseParams.session_id = session_id;",
                         ##              "loader.baseParams.path = nodeToPath(node).value.ids;",
                         ##              "});",
                         ##              sep="")
                         ## add_js_queue(cmd)

                         ## we want to remove items when we collapse, but this doesn't work. Just collapse
                         cmd <- paste(sprintf("%s.on('beforecollapsenode', function(node, deep, anim) {", get_id()),
                                      "node.collapseChildNodes(true);",
                                      "})",
                                      sep="")
### XXX                          add_js_queue(cmd)

                       },
                       transport_fun = function() {
                         ## we traverse to find the ids (0-based)
                         "sel=selected[0];var param = nodeToPath(sel);"
                       },
                       process_transport = function(value, ...) {
                         ## transports in a list with id and path
                         value <<- value$index + 1 # value is index
                         x <- value$ids
                         ## need to work on this:
                         paths <- strsplit(x, ":\\.:")[[1]]
                         out <- lapply(paths, function(x) strsplit(x, ":")[[1]][-1])
                         path <<- out[[length(out)]]
                         cur_record <<- value$record
                       },
                       update = function(...) {
                         "Update tree"
                         cmd <- paste(sprintf("var rootNode = %s.getRootNode();", get_id()),
                                      sprintf("%s.getLoader().load(rootNode);", get_id()),
                                      "rootNode.expand();",
                                      sep="")
### XXX                         add_js_queue(cmd)
                       },
                       get_value = function(drop=TRUE,...) {
                         "Get selected value, in trees case a path or path indices"
                         if(drop)
                           tail(path, n=1)
                         else
                           path
                       },
                       get_index=function(drop=TRUE, ...) {
                         value
                       },
                       get_items=function(...) {
                         "Return record of currently selected row"
                         cur_record
                       },
                       set_value = function(value, ...) {
                         "Set value based on path, eg. c(1,2,1). Fires 'click' event to update path, value"
                         cmd <- paste(sprintf("var a = %s;", toJSArray(value - 1)),
                                      "var k = a.length;",
                                      sprintf("var n = %s.getRootNode();", get_id()),
                                      "Ext.each(a.slice(0, a.length-1), function(i) {",
                                      "  n = n.childNodes[i];",
                                      "  n.expand(false)",
                                      "});",
                                      "n = n.childNodes[a[a.length-1]];",
                                      ##"n.select();", ## XXX How to select last node
                                      ##"n.fireEvent('click',Ext.apply(n, {node:n}));",
                                      sep="")
                         add_js_queue(cmd)
                       },
                       set_index=function(value, ...) {
                         set_value(value, ...)
                       },
                       ## handlers
                       add_handler_changed = function(...) {
                         "Main handler is doubleclick. Single click is for selection, double ofr action"
                         add_handler_doubleclick(...)
                       },
                       add_handler_doubleclick = function(...) {
                         add_R_callback("dblclick", ...)
                       }
                       
                       
                       ))
 
                         
