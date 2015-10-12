##      Copyright (C) 2011  John Verzani
##      Copyright (C) 2015  Johannes Ranke (port to R6)
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
##' A widget for editing a data frame. This implementation allows the
##' user to edit a row (row-by-row). Change handlers are called
##' after an edit is performed. The user can add a new row or remove
##' the currently selected row (if more than 1 is present). The hidden
##' argument \code{do_add_remove_buttons} can be set to \code{FALSE}
##' to prevent this option. For large data sets, paging is used. the
##' hidden argument \code{page_size} (with default of 200) can be set
##' to adjust the size of each page.
##' @param items data frame to be edited
##' @param name name of data frame appearing in title
##' @inheritParams gwidget
##' @export
##' @return a \code{GDf} reference class object.
##' @author john verzani
gdf <- function(items = NULL,
                name = deparse(substitute(items)),
                handler=NULL,
                action=NULL,
                container = NULL, ...,
                width=300, height=200,   # gWidgetsWWW defaults
                ext.args = NULL
                ) {

  gd <- GDf$new(container$toplevel)
  gd$init(items, name, handler, action, container, ..., 
          width=width, height=height, ext.args=ext.args)
  gd
}

GDf <- R6Class("GDf",
  inherit = GWidgetGrid,
  public = list(
    name = NA,
    ## @param page_size size of page request to
    ## server. Smaller means faster but more frequent
    ## @param setType type of selection for editing:
    ##entire row or single cell
    init = function(items, name, handler, action, container, ..., 
                    width=NULL, height=NULL, ext.args=NULL,
                    page_size=200L, do_add_remove_buttons=TRUE
                    #, selType=c("rowmodel", "cellmodel")
                    ) {

      if(!is.data.frame(items))
        items <- as.data.frame(items, stringsAsFactors=FALSE)

      self$store <- GWidgetArrayStore$new(container$toplevel, extra_args=list(autoSync=TRUE))
      self$store$init(items)

      self$name <- name
      self$nms <- names(items)

      ## Hack alert
      ## set default height/width if missing and needed
      if(is(container, "GGroup")) {
        expand <- getFromDots("expand", ..., NULL)
        if(is.null(expand) || !as.logical(expand)) {
          if(container$horizontal)
            width <- getWithDefault(width, 300L)
          else
            height <- getWithDefault(height, 200L)
        }
      }
      
      self$constructor <- "Ext.grid.Panel"
      ## Transport is handled by Ext through the Ajax proxy, no need here
      ## transport_signal <<- "afteredit"
      self$change_signal <- "edit"
      
      arg_list = list(
        store = String(self$store$get_id()),
        columns = String(self$store$proxy$make_column_model(do.edit=TRUE)),
        stripeRows = TRUE,
        frame = FALSE,
        title = name,
        width = width,
        height = height,
        selType = "rowmodel",
        #selType=match.arg(selType),
        #plugins=String("[Ext.create('Ext.grid.plugin.CellEditing', {clicksToEdit: 1})]"),
        plugins = String(sprintf("[%s]", self$roweditor_id())),
        paging_options <- list(
                               pageSize = as.integer(page_size),
                               store = String(self$store$get_id()),
                               displayInfo = TRUE,
                               displayMsg = gettext("Displaying rows {0} - {1} of {2}"),
                               emptyMsg = gettext("No rows to display")
                               )
      )
      arg_list[['bbar']] <- String(self$paging_id())
      
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
      add_cmd <- whisker.render(add_tpl, 
                                list(row = self$roweditor_id(), 
                                     proxy = self$store$proxy$get_id(), 
                                     model = self$store$model_id(), 
                                     store = self$store$get_id(), 
                                     page = self$paging_id()))

      ## remove from store, then refresh. Do not remove via remove JS method
      remove_tpl <- "
function() {
  var selection={{id}}.getSelectionModel().getSelection()[0];
  if(selection) {
    var rowidx = selection.raw[0];
    var count = {{store}}.getTotalCount();
    jRpc('{{proxy}}', 'remove_row', {row:rowidx}, function() {
      {{page}}.doRefresh();
    })

  }
}
"
      ## bonepile -- remove if not needed
      ## jRpc('{{proxy}}', 'remove_row', {row:rowidx}, function() {
      ##   {{proxy}}.suspendEvents();
      ##   {{store}}.totalCount = count - 1;
      ##   {{proxy}}.resumeEvents();
      ##   {{page}}.doRefresh();
      ## })
      ##       {{store}}.insert(count, r);

      remove_cmd <- whisker.render(remove_tpl,
                                   list(id = self$get_id(), 
                                        proxy = self$store$proxy$get_id(),
                                        store = self$store$get_id(), 
                                        page = self$paging_id()))

      ## put in add and remove buttons
      if(do_add_remove_buttons)
        arg_list[['tbar']] <- String(
          sprintf("[{text:'Add', handler:%s}, {text:'Remove', handler:%s,id:'%s_delete', disabled: true}]", 
                  add_cmd, remove_cmd, self$get_id()))

      self$add_args(arg_list)

      self$store$page_size <- as.integer(page_size)

      # JR: Not sure if paging_options belongs into arg_list
      #cmd <- sprintf("var %s = new Ext.PagingToolbar(%s);", self$paging_id(), toJSObject(paging_options))
      cmd <- sprintf("var %s = new Ext.PagingToolbar(%s);", 
                     self$paging_id(), 
                     toJSObject(arg_list$paging_options))
      self$add_js_queue(cmd)
      
      ## write row editor
      cmd <- sprintf("var %s=Ext.create('Ext.grid.plugin.RowEditing', {clicksToMoveEditor: 1,autoCancel: false});", 
                     self$roweditor_id())
      self$add_js_queue(cmd)
      
      self$setup(container, handler, action, ext.args, ...)

      ## delete if a selection and more than 1 row
      if(do_add_remove_buttons) {
        cmd <- whisker.render("
{{id}}.getSelectionModel().on('selectionchange', function(selModel, selections) {
  ( {{id}}.getStore().getTotalCount() > 1 ) &&
  {{id}}.down('#{{id}}_delete').setDisabled(selections.length === 0)
});
",
          list(id = self$get_id())
        )
        self$add_js_queue(cmd)
      }

      ## XXX This is annoying. For some reason after
      ## editing a new row is left repeating the
      ## edited row. Clearly not desirable. This hits
      ## the server one more time than is needed
      cmd <- paste(sprintf("%s.on('edit', function(e, obj) {",roweditor_id()),
                   sprintf("callRhandler('%s', 'edit', null);", get_id()),
                   sprintf("%s.doRefresh();", paging_id()),
                   sprintf("});"),
                   sep="")
##      add_js_queue(cmd)
      
      ## handler


      ## load data
      self$store$load_data()
    },
    roweditor_id = function() {
      sprintf("%s_roweditor", self$get_id())
    },
    paging_id = function() {
      sprintf("%s_paging_toolbar", self$get_id())
    },
    ## can toggle editability of columns
    ## XXX Working? 
    set_editable = function(value, column) {
      "Set a column editable or not. @param value logical, @param column column number, Defaults to all"
      if(missing(column))
        column <- 1:dim()[2] # JR: dim() should not be called without arguments
      sapply(column, function(col) {
        self$call_column_method("setEditable", col - 1, value)
      })
    },
    set_title = function(value) {
     "Reference method to set title naming data frame"
     self$call_Ext("setTitle", value)
    },
    hide_title=function(value) {
      
    }#,
#                     ## bypass, connected to "edit" signal above
#                     add_R_callback=function(...) {}
  )
)
