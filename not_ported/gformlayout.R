##      Copyright (C) 2012  John Verzani
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

##' @include gcontainer.R
NULL

##' A form layout container
##'
##' A simple way to layout forms with labels.  The label is passed to
##' the \code{label} argument of the control's constructor which is
##' then passed along to the \code{add} method of the parent
##' container.
##' @param align alignment of labels, from \code{c("default", "left",
##' "center", "right", "top")}.
##' @param spacing spacing between columns
##' @param container parent container
##' @param ... passed to \code{add} method of parent container.
##' @param label.width width reserved for labels
##' @inheritParams gwidget
##' @export
##' @examples
##' \dontrun{
##' w <- gwindow("gformlayout")
##' g <- gvbox(container=w)
##' 
##' flyt <- gformlayout(container=g)
##' gedit("", label="Name:", container=flyt)
##' gedit("", label="Rank:", container=flyt)
##' gedit("", label="Serial No.:", container=flyt)
##' 
##' b <- gbutton("Show me", container=g, handler=function(h,...) {
##' print(svalue(flyt))
##' })
##' 
##' }
gformlayout <- function(
                        align=c("default", "left", "center", "right", "top"),
                        spacing=5,
                        container = NULL, ...,
                        label.width=100,
                        width=NULL, height=NULL, ext.args=NULL){
 g <- GFormLayout$new(container, ...)
 g$init(align=align, spacing=spacing, container=container, ...,
        label.width=label.width,
        width=width, height=height, ext.args=ext.args)
 g
}


GFormLayout <- setRefClass("GFormLayout",
                           contains="GContainer",
                           fields=list(
                             coerce_with="ANY"
                             ),
                           methods=list(
                             init = function(
                               align=c("default", "left", "center", "right", "top"),
                               spacing=2, 
                               container,
                               ...,
                               label.width=100L,
                               width =NULL,
                               height=NULL,
                               ext.args = NULL
                               ) {

                               constructor <<- "Ext.form.Panel"

                               arg_list <- list(bodyPadding=spacing,
                                                labelSeparator="",
                                                labelAlign=c("default"="left",
                                                  "left"="left",
                                                  "center"="center",
                                                  "right"="right",
                                                  "top"="top")[match.arg(align)],
                                                defaults=list(
                                                  labelWidth=label.width
                                                  )

                                                )
                               add_args(arg_list, ext.args)
                               
                               container$add_dots(.self, ...)
                               write_constructor()
                               container$add(.self, ...)
                               set_size(list(width=width, height=height))
                             },
                             get_value=function(...) {
                               ## return named values. Real hacky, as we
                               ## are deailing with Array
                               vals <- lapply(.self$children$l, svalue)
                               nms <- sapply(.self$children$l, function(i) {
                                 i$args$args$l$fieldLabel
                               })
                               names(vals) <- nms
                               vals
                             },
                             get_items=function( ...) {
                               "Return widgets iwth names"
                               vals <- .self$children$l
                               nms <- sapply(vals, function(i) {
                                 i$args$args$l$fieldLabel
                               })
                               names(vals) <- nms
                               vals
                             }
                             ))
