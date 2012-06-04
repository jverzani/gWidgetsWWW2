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

##' @include gcontainer.R
NULL

##' A "border" layout is a 5-panel layout where  satellite panels
##' surround a "center" panel. 
##'
##' The \code{gborderlayout} container implements a border layout
##' where 4 panels surround a main center panel. The panels may be
##' configured with a title (like \code{gframe}) and may be
##' collapsible (like \code{gexpandgroup}). Both configurations are
##' done at construction time. The panels only hold one child, so one
##' would add a container to have more complicated layouts.
##'
##' To add a child component, one specifies a value of \code{where} to
##' the \code{add} method (implicitly called by the constructor, so in
##' practice this argument is passed through \code{...} by the
##' constructor). The value of \code{where} is one of
##' \code{c("center","north", "south", "east", "west")}. Child
##' components are added with the "fit" layout, which is basically the
##' same as specifying \code{expand=TRUE} and \code{fill=TRUE}, though those
##' arguments are ignored here.
##' 
##' The satellite panels may be resized through the reference class
##' method \code{set_panel_size} with arguments \code{where} and a
##' numeric \code{dimension}. 
##' @inheritParams gwidget
##' @param title a list  with named components from
##' \code{c("center","north", "south", "east", "west")} allowing one
##' to specify titles (as length-1 character vectors) of the
##' regions. The default is no title. A title may be added later by
##' adding a \code{gframe} instance, but that won't work well with a
##' collapsible panel.
##' @param collapsible a list with  named components from
##' \code{c("center","north", "south", "east", "west")} allowing one
##' to specify through a logical if the panel will be collapsible,
##' similar to \code{gexpandgroup}. The default is \code{FALSE}
##' @return a \code{GBorderLayout} reference class object
##' @seealso \code{\link{gpanedgroup}} is a two-panel border layout
##' with just an "east" or "south" satellite panel configured.
##' @note \code{gpanedgroup} does not sit nicely within a
##' \code{gnotebook} container, avoid trying this.
##' @author john verzani
##' @export
##' @examples
##' w <- gwindow("border layout")
##' #
##' bl <- gborderlayout(cont=w,
##'                     title=list(center="State facts (state.x77)", west="Select a state"),
##'                     collapsible=list(west=TRUE)
##'                     )
##' #
##' tbl <- gtable(data.frame(states=rownames(state.x77), stringsAsFactors=FALSE),
##'               multiple=FALSE,
##'               cont=bl, where="west")
##' size(tbl) <- c("100%", "100%")
##' bl$set_panel_size("west", 200)
##' #
##' g <- ggroup(cont=bl, where="center", horizontal=FALSE, height=500, width=500)
##' nms <- colnames(state.x77)
##' 
##' labs <- lapply(seq_along(nms), function(i) {
##'   g1 <- ggroup(cont=g, width=500)
##'   glabel(sprintf("<b>%s</b>",nms[i]), cont=g1)
##'   glabel("", cont=g1)
##' })
##' #
##' update_state_info=function(h,...) {
##'   nm <- svalue(h$obj)
##'   nm <- sample(rownames(state.x77),1)
##'   facts <- state.x77[nm,]
##'   sapply(seq_along(facts), function(i) {
##'     lab <- labs[[i]]
##'     svalue(lab) <- facts[i]
##'   })
##' }
##' addHandlerChanged(tbl, handler=update_state_info)
gborderlayout <- function(container=NULL, ...,
                          width=NULL,
                          height=NULL,
                          ext.args=NULL,
                          title=list(),
                          collapsible=list()
                          ) {

  obj <- GBorderLayout$new(container, ...)
  obj$init(
           container,
           ...,
           width=width,
           height=height,
           ext.args=ext.args,
           title=title,
           collapsible=collapsible
          )
  obj
}

GBorderLayout <- setRefClass("GBorderLayout",
                             contains="GContainer",
                             methods=list(
                              init=function(container, ...,
                                width=NULL, height=NULL,
                                ext.args=NULL,
                                title,
                                collapsible
                                ) {

                                constructor <<- "Ext.panel.Panel"
                                arg_list <- list(
                                                 width=width,
                                                 height=height,
                                                 layout="border",
                                                 items=String(sprintf("[%s]", make_panels(title, collapsible)))
                                                 )

                                
                                args$extend(arg_list, ext.args)
                                write_constructor()
                                container$add(.self, ...)
                                
                                if(is.null(width) && is.null(height))
                                  set_size(c("100%", "100%"))

                              },
                              region_id=function(where=c("center","north", "south", "east", "west")) {
                                "Return ID of region"
                                where <- match.arg(where)
                                sprintf("%s_%s_region", get_id(), where)
                              },
                              make_panels=function(title, collapsible) {
                                where <- c("center","north", "south", "east", "west")
                                out <- lapply(where, function(i) {
                                  lst <- list(region=i,
                                              xtype="panel",
                                              layout="fit",
                                              title=title[[i]],
                                              collapsible=collapsible[[i]],
                                              split=TRUE,
                                              margins="0,5,5,5",
                                              id=region_id(i)
                                              )
                                  toJSObject(lst)
                                })
                                paste(out, collapse=",")
                              },
                              add=function(child,
                                where=c("center","north", "south", "east", "west"),
                                ...
                                ) {
                                "add child to specific region, defaulting to center"
                                child_bookkeeping(child)
                               
                                tpl <- "
wrc = Ext.getCmp('{{region_id}}');
wrc.removeAll();
wrc.add({{child_id}});
"
                                cmd <- whisker.render(tpl,
                                               list(region_id=region_id(where=where),
                                                    child_id=child$get_id()))
                                add_js_queue(cmd)
                              },

                              set_panel_size=function(where=c("center","north", "south", "east", "west"), dimension) {
                                "@param  where which panel (center, north, ...), @param dimension width or height as appropriate"
                                ## XXX What to do with center?
                                where <- match.arg(where)
                                meth <- ifelse(where %in% c("east", "west"), "setWidth", "setHeight")

                                tpl <- "
wrc = Ext.getCmp('{{region_id}}');
wrc.{{meth}}({{dimension}});
"
                                cmd <- whisker.render(tpl, list(region_id=region_id(where=where),
                                                                meth=meth,
                                                                dimension=dimension))
                                add_js_queue(cmd)
                              },
                              set_panel_collapse=function(where=c("center","north", "south", "east", "west"), collapse=TRUE) {
                                "collapse or expand (collapse=FALSE) collapsible panel"
                                where <- match.arg(where)
                                meth <- ifelse(collapse, "collapse", "expand")
                                
                                tpl <- "
wrc = Ext.getCmp('{{region_id}}');
wrc.{{meth}}();
"
                                cmd <- whisker.render(tpl, list(region_id=region_id(where=where),
                                                                meth=meth))
                                add_js_queue(cmd)

                              }
                              ))
