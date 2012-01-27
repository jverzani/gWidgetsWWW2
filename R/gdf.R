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

##' @include gtable.R
NULL


##' A widget for editing a data frame
##'
##' A widget for editing a data frame. There is no means to change the
##' size of the frame being edited or the type of data in each
##' column. As such, one must plan ahead. This implementation allows
##' one to add a row to the data store, to edit a row (row-by-row),
##' but there is no means to delete a row.
##' @param items data frame to be edited
##' @param name name of data frame appearing in titke
##' @param do.subset Ignored.
##' @inheritParams gwidget
##' @return a \code{GDf} reference class object.
##' @author john verzani
gdf <- function(items = NULL, name = deparse(substitute(items)),
                do.subset = FALSE,
                container = NULL, ...,
                width=300, height=200,   # gWidgetsWWW defaults
                ext.args = NULL
                ) {

  gd <- GDf$new(container$toplevel)
  gd$init(items, name, container, ..., width=width, height=height, ext.args=ext.args)
  gd
}

## base class for gdatarframe
GDf <- setRefClass("GDf",
                   contains="GWidgetGrid",
                   fields=list(
                     "store"= "ANY",
                     "name" = "character"
                     ),
                   methods=list(
                     ## @param page_size size of page request to
                     ## server. Smaller means faster but more frequent
                     ## @param setType type of selection for editing:
                     ##entire row or single cell
                     init=function(items, name, container, ..., width=NULL, height=NULL, ext.args=NULL,
                       page_size=200L
                       #, selType=c("rowmodel", "cellmodel")
                       ) {

                       if(!is.data.frame(items))
                         items <- as.data.frame(items, stringsAsFactors=FALSE)

                       store <<- GWidgetArrayStore$new(container$toplevel, extra_args=list(autoSync=TRUE))
                       store$init(items)

                       name <<- name
                       nms <<- names(items)
                       
                       constructor <<- "Ext.grid.Panel"
                       ## Transport is handled by Ext through the Ajax proxy, no need here
                       ## transport_signal <<- "afteredit"
                       change_signal <<- "edit"
                       
                       arg_list = list(
                         store = String(store$get_id()),
                         columns = String(store$proxy$make_column_model(do.edit=TRUE)),
                         stripeRows = TRUE,
                         frame = FALSE,
                         title = name,
                         width=width,
                         height=height,
                         selType="rowmodel",
                         #selType=match.arg(selType),
                         #plugins=String("[Ext.create('Ext.grid.plugin.CellEditing', {clicksToEdit: 1})]"),
                         plugins=String(sprintf("[%s]", roweditor_id())),
                         paging_options <- list(
                                                   pageSize= as.integer(page_size),
                                                   store= String(store$get_id()),
                                                   displayInfo=TRUE,
                                                   displayMsg= gettext("Displaying rows {0} - {1} of {2}"),
                                                   emptyMsg= gettext("No rows to display")
                                                   )
                         )
#                       cmd <- sprintf("new Ext.PagingToolbar(%s)", toJSObject(paging_options))
#                       arg_list[['bbar']] = String(cmd)
                       arg_list[['bbar']] <- String(paging_id())

##       {{store}}.insert(count, r);
                       
                       add_tpl <- "
function() {
  {{row}}.cancelEdit();
  var r = Ext.create('{{model}}', {});
  var count = {{store}}.getTotalCount();
  {{store}}.totalCount = count + 1;
  jRpc('{{proxy}}', 'add_row', {row:count + 1}, function() {
      {{page}}.doRefresh();
      {{row}}.startEdit(count+1, 0);
    })
}
"
                       add_cmd <- whisker.render(add_tpl, list(row=roweditor_id(), proxy=store$proxy$get_id(), model=store$model_id(), store=store$get_id(), page=paging_id()))
                       ## add_cmd <- paste("function() {",
                       ##              sprintf("%s.cancelEdit();", roweditor_id()),
                       ##              sprintf("var r=Ext.create('%s',{});", store$model_id()),
                       ##              sprintf("var count=%s.getTotalCount();", store$get_id()),
                       ##              sprintf("%s.insert(count, r);", store$get_id()),
                       ##              sprintf("%s.totalCount = count + 1;", store$get_id()),
                       ##              sprintf("%s.doRefresh();", paging_id()),
                       ##              sprintf("%s.startEdit(count+1,0);", roweditor_id()),
                       ##              "}", sep="")
                       remove_tpl <- "
function() {
  var selection={{id}}.getSelectionModel().getSelection()[0];
  if(selection) {
    var rowidx = selection.raw[0];
    jRpc('{{proxy}}', 'remove_row', {row:rowidx}, function() {
      {{proxy}}.suspendEvents();
      {{store}}.remove(selection);
      {{proxy}}.resumeEvents();
      {{page}}.doRefresh();
    })
  }
}
"
                      remove_cmd <- whisker.render(list(id=get_id(), proxy=store$proxy$get_id(),
                                                        store=store$get_id(), page=paging_id()))

         ## remove_cmd <- paste("function() {",
         ##                                   sprintf("var selection=%s.getSelectionModel().getSelection()[0];", get_id()),
         ##                                   sprintf("if(selection) {"),
         ##                                   sprintf("rowidx=selection.raw[0];"),
         ##                                   sprintf("jRpc('%s','remove_row',{row:rowidx},function() {", store$proxy$get_id()),
         ##                                   sprintf("%s.suspendEvents(false);%s.remove(selection);%s.resumeEvents();});", store$proxy$get_id(), store$get_id(), store$proxy$get_id()),
         ##                                   sprintf("%s.doRefresh();", paging_id()),
         ##                                   "}}",
         ##                                   sep="")
                       ## We only had add -- not remove
#                       arg_list[['tbar']] <- String(sprintf("[{text:'Add', handler:%s}, {text:'Remove',handler:%s,id:'%s_delete', disabled: true}]", add_cmd, remove_cmd, get_id()))
                       arg_list[['tbar']] <- String(sprintf("[{text:'%s', handler:%s}]", gettext("Add row"), add_cmd))
                       
                       add_args(arg_list)
                       store$page_size <<- as.integer(page_size)

                       cmd <- sprintf("var %s = new Ext.PagingToolbar(%s);", paging_id(), toJSObject(paging_options))
                       add_js_queue(cmd)
                       
                       ## write row editor
                       cmd <- sprintf("var %s=Ext.create('Ext.grid.plugin.RowEditing', {clicksToMoveEditor: 1,autoCancel: false});", roweditor_id())
                       add_js_queue(cmd)
                       
                       setup(container, NULL, NULL, ext.args, ...)

##                        ## delete if a selection.
##                        cmd <- sprintf("%s.getSelectionModel().on('selectionchange', function(selModel, selections){
## %s.down('#%s_delete').setDisabled(selections.length === 0)});", get_id(),get_id(), get_id())
##                        add_js_queue(cmd)

                       
                       ## XXX This is annoying. For somer reason after
                       ## editing a new row is left repeating the
                       ## edited row. Clearly not desirable. This hits
                       ## the server one more time than is needed
                       cmd <- paste(sprintf("%s.on('edit', function(e, obj) {",roweditor_id()),
                                    sprintf("callRhandler('%s', 'edit');", get_id()),
                                    sprintf("%s.doRefresh();", paging_id()),
                                    sprintf("});"),
                                    sep="")
                       add_js_queue(cmd)
                       
                       
                       ## load data
                       store$load_data()
                     },
                     roweditor_id=function() {
                       sprintf("%s_roweditor", get_id())
                     },
                     paging_id=function() {
                       sprintf("%s_paging_toolbar", get_id())
                     },
                     ## can toggle editability of columns
                     ## XXX Working?
                     set_editable = function(value, column) {
                       "Set a column editable or not. @param value logical, @param column column number, Defaults to all"
                       if(missing(column))
                         column <- 1:dim()[2]
                       sapply(column, function(col) {
                         call_column_method("setEditable", col - 1, value)
                       })
                     },
                     ## bypass, connected to "edit" signal above
                     add_R_callback=function(...) {}
                    
                       
                     ))


## We need to coerce a value from string to ..


## ##' Generic to coerce value before assigning into data frame
## ##' @param x what value will go into
## ##' @param value to set into x
## coerceValue <- function(x, value) UseMethod("coerceValue")

## ##' method to coerce value before assigning into data frame
## ##' @param x what value will go into
## ##' @param value to set into x
## coerceValue.default <- function(x, value) format(value)

## ##' method to coerce value before assigning into data frame
## ##' @param x what value will go into
## ##' @param value to set into x
## coerceValue.character <- function(x, value) as.character(value)

## ##' method to coerce value before assigning into data frame
## ##' @param x what value will go into
## ##' @param value to set into x
## coerceValue.integer <- function(x, value) as.integer(value)

## ##' method to coerce value before assigning into data frame
## ##' @param x what value will go into
## ##' @param value to set into x
## coerceValue.numeric <- function(x, value) as.numeric(value)

## ##' method to coerce value before assigning into data frame
## ##' @param x what value will go into
## ##' @param value to set into x
## coerceValue.logical <- function(x, value)  as.logical(toupper(value))

## ##' method to coerce value before assigning into data frame
## ##' @param x what value will go into
## ##' @param value to set into x
## coerceValue.factor <- function(x, value) ifelse(value %in% levels(x), value, NA)
