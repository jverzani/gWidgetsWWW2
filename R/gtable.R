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

##' @include gwidget.R
##' @include gwidget-proxy.R
##' @include icons.R
NULL

##' A table widget
##'
##' A widget for displaying a data frame in tabular format. The main
##' property is the set of indices for the currently selected
##' rows. For large data sets, the data can be "paged", that is given
##' to the browser in bite-sized chunks so the lag is lacking.  The
##' change handler is for a single click, also used for selection. Use
##' \code{addHandlerDoubleclick} to specify a callback for the double
##' click event on a cell.
##' 
##' The column names are inherited from the
##' columnnames of the data frame. 
##' 
##' A column of class "Icon" (see
##' \code{\link{asIcon}}) will render a css class as an icon. See the
##' example.
##' 
##' The item replacement operator \code{[<-} will work
##' only if all the column types remain constant, as the column
##' renderers are set at time of construction. This also effects the
##' initial data frame. Pass a 0-row data frame with column names and
##' defined column types at construction if no data is known at that
##' point.
##' @param items data frame of items to display
##' @param multiple logical. TRUE for multiple selections
##' @param chosencol The svalue() method returns a single value, by default. This species column of that value.
##' @param icon.FUN NOT IMPLEMENTED. Instead, just add a column with class Icon containing css class of the icons
##' @param filter.column Ignored 
##' @param filter.labels Ignored 
##' @param filter.FUN Ignored. 
##' @param handler single click handlers
##' @param ext.args additional configuration values to pass to constructor
##' @param paging Either a logical variable or integer. If \code{TRUE}
##' then paging will be used which allows only chunks of the data to
##' be sent to the browser at a time (default size = 200 rows). If
##' \code{integer} then paging is turned on and this value overrides
##' the default page size.
##' @param col.widths width of each column. Also see \code{size<-}
##' with a list where \code{columnWidths} is specified.
##' @inheritParams gwidget
##' @return An ExtWidget instance
##' @note With \code{width} and/or \code{height} set to \code{NULL},
##' the default size will likely be unsatisfying. (And can consume any
##' space in a box, so items packed in after will not be shown.) As
##' such, these values are often best set by the programmer. They can
##' be readjusted through the \code{size<-} method. The \code{size<-}
##' method can also be used to adjust the columns widths, by passing a
##' list with a component named \code{columnWidths} containing the
##' desired widths in pixels.
##'
##' The \code{visible<-} method may be used for filtering.
##' @export
##' @examples
##' w <- gwindow("Filtering and table example")
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
##' g <- ggroup(cont=w, horizontal=FALSE)
##' g1 <- ggroup(cont=g)
##' glabel("Filter by:", cont=g1)
##' e <- gedit("", cont=g1)
##' tbl <- gtable(data.frame(name=state.name, stringsAsFactors=FALSE), cont=g,  multiple=TRUE)
##' addHandlerKeystroke(e, handler=function(h,...) {
##' val <- svalue(h$obj)
##' if(nchar(val) > 0) {
##' tbl$filter("name", val)
##' }
##' })
##' b <- gbutton("click", cont=g, handler=function(h,...) galert(svalue(tbl, index=FALSE), parent=w))
##' 
##' ## icons
##' m <- mtcars[1:3, 1:4]
##' ## add icons as css class
##' m[,1] <- asIcon(c("up","up","down"))
##'  gtable(m, cont=g)
gtable <- function(items, multiple = FALSE, chosencol = 1,
                   icon.FUN = NULL,
                   filter.column = NULL, filter.labels = NULL,
                   filter.FUN = NULL, handler = NULL, action = NULL,
                   container = NULL, ...,
                   width=NULL, height=NULL, ext.args=NULL,
                   paging = FALSE, 
                   col.widths = rep(20, ncol(as.data.frame(items)))
                   ) {

  tbl <- GTable$new(container, ...)
  tbl$init(items, multiple, chosencol, icon.FUN, handler, action, container,
           width=width, height=height, ext.args=ext.args, paging=paging, col.widths=col.widths, ...)
  tbl
}


## A class inherited by GTable and GDf
GWidgetGrid <- R6Class("GWidgetGrid",
  inherit = GWidget,
  public = list(
    store = NA,
    nms = NA, ## column name
    ## put common methods here
    ## set__items is in subclass
    get_items = function(i, j, ..., drop=TRUE) {
      items <- self$store$proxy$get_data(drop_visible=FALSE)
      items[i,j, ..., drop=drop]
    },
    set_items = function(value, i, j, ...) {
      if(missing(i) && missing(j)) {
        self$store$set_data(value)
      } else {
        items <- self$store$get_data()
        items[i,j] <- value
        self$store$set_data(items)
      }
      self$nms <- names(self$store$proxy$value)
      self$store$load_data()
    },
    set_size = function(val, ...) {
      "Set size of table (width,height) or columnWidths"
      width <- height <- colWidths <- NULL
      val <- as.list(val)
      if(is.list(val)) {
        width <- val$width
        height <- val$height
        colWidths <- val$columnWidths
        if(!is.null(colWidths))
          self$set_column_widths(colWidths)
      }
      if(is.null(width) && is.null(height))
        return()
      else if(is.null(height))
        self$call_Ext("setWidth", width)
      else if(is.null(width))
        self$call_Ext("setHeight", height)
      else
        super$set_size(c(width, height))
    },
    get_dim = function() {
      base::dim(self$get_items())
    },
    len = function(x) {
      "Length of data. For convenience, if passed an argument gives length of that"
      if(missing(x))
        base::length(self$get_items())
      else
        base::length(x)
    },
    ## Some column methods
    call_column_method = function(meth, ind, ...) {
      "Call a method of the column model, like call_Ext"
      l <- list(...)
      out <- sapply(l, coerceToJSString)
      cmd <- sprintf("%s.columns[%s].%s(%s);",
                     self$get_id(),
                     ind - 1,
                     meth,
                     paste(out, collapse=","))
      self$add_js_queue(cmd)
    },
    set_column_name = function(column, value) {
      self$call_column_method("setText", column, value)
    },
    get_names = function() {
      self$nms
    },
    set_names = function(value) {
      self$nms <- value
      mapply(self$set_column_name, seq_along(value), value)
    },
    set_column_width = function(column, value) {
      self$call_column_method("setWidth", column, value)
    },
    set_column_widths = function(value) {
      mapply(self$set_column_width, seq_along(value), value)
    },
    ## Not there in ExtJS 4.1? JR: Indeed seems not to be available
    ## set_column_tooltip = function(value, column) {
    ##   "Set tooltop for specified column"
    ##   call_column_method("setColumnTooltip", column - 1, value)
    ## },
    ## set_column_tooltips = function(value) {
    ##   "Set tooltips for entire set of header columns"
    ##   sapply(seq_along(value), function(i) {
    ##     set_column_tooltip(value[i], i)
    ##   })
    ## },
    ## ## handlers
    add_handler_selection_changed=function(...) {
      self$add_handler("selectionchange", ...)
    },
    add_handler_clicked = function(...) {
      self$add_handler("cellclick", ...)
    },
    add_handler_double_click = function(...) {
      self$add_handler("celldblclick", ...)
    },
    add_handler_column_clicked = function(...) {
      self$add_handler("headerclick", ...)
    },
    add_handler_column_double_click = function(...) {
      self$add_handler("headerdblclick", ...)
    } 
  )
)

##' \code{GTable} is the base class for gtable
##'
##' The table widget is implemented using a proxy. That is, the data
##' is loaded in a separate AJAX call. This makes things relatively
##' responsive, but if there is too much data one can turn on paging.
##'
##' The widget can filter through the visible method. This basically
##' passes back the filtered data from the server each time it is
##' called. To avoid the data transfer, one can use the \code{filter}
##' reference method, which filters browser side by a regular
##' expression.
##' @rdname gtable
GTable <- R6Class("GTable",
  inherit = GWidgetGrid,
  public = list(
    multiple = logical(),
    chosencol = integer(),
    paging = logical(),
    page_size = integer(),
    ##' @param col.widths vector with column widths
    init = function(items, multiple, chosencol, icon.FUN, handler, action, container,...,
                    width=NULL, height=NULL, ext.args=NULL, paging=nrow(items) > 200,
                    col.widths
                    ) {

      ## coerceitems
      if(!is.data.frame(items))
        items <- as.data.frame(items, stringsAsFactors=FALSE)
      
      self$value <- NA  # currently selected row(s) or NA
      self$multiple <- multiple
      self$chosencol <- as.integer(chosencol)

      ## Paging is used when the store has many rows. ideally
      ## we would like to use the infinite scrolling feature,
      ## but this isn't working for us.
      ## The issue below is when paging is FALSE. We can't
      ## set the page size dynamically, so we just crank up
      ## a big one.
      ## This is only an issue if items is initially small but
      ## will be swapped out with something big.
      def_page_size <- 200L
      if(is.logical(paging)) {
        self$paging <- paging
        if(paging) {
          self$page_size <- def_page_size ## override through assignment paging=2000
        } else {
          self$page_size <- 2000L
        }
      } else {
        self$page_size <- as.integer(paging)
        self$paging <- TRUE
      }

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

      self$store = GWidgetArrayStore$new(container)
      self$nms = names(items)
      self$constructor = "Ext.grid.Panel"
      self$transport_signal = "selectionchange"
      self$change_signal = "selectionchange"
       
      self$store$init(items, page.size = self$page_size)
      self$store$paging <- self$paging
      self$store$page_size <- self$page_size
      
      arg_list = list(
        store = String(self$store$get_id()),
        columns = String(self$store$proxy$make_column_model()),
        stripeRows = TRUE,
        ## selType="rowmodel",
        frame = FALSE,
        autoExpandColumn = tail(names(items), n=1),
        width = width,
        height = height,
        sortableColumns = TRUE
        )
      if(multiple)
        arg_list$multiSelect <- TRUE
      
      if(!paging) {
        arg_list <- merge.list(arg_list, 
                               list(autoLoad=FALSE,
                                    verticalScroller=list(
                                      trailingBufferZone=200,
                                      leadingBufferZone=500
                                      )
                                    ))
      } else if(paging) {
        self$store$page_size <- as.integer(page_size)
        paging_options <- list(
                               pageSize= as.integer(page_size),
                               displayInfo=TRUE,
                               displayMsg= gettext("Displaying rows {0} - {1} of {2}"),
                               emptyMsg= gettext("No rows to display")
                               )
        cmd <- sprintf("new Ext.PagingToolbar(%s)", toJSObject(paging_options))

        arg_list$dockedItems = String(
          sprintf("[{xtype:'pagingtoolbar', store:%s,dock:'bottom',displayInfo:true}]", 
                  store$get_id()))
##        arg_list[['bbar']] = String(cmd)
      }

      ## hacks!
      ## issue with height=NULL
      
      self$add_args(arg_list)
      self$setup(container, handler, action, ext.args, ...)
      
      ## set up paging
      if(paging) { ## adjust size
        cmd <- sprintf("%s.getTotalCount = function() {return %s};",
                       self$store$get_id(), nrow(self$store$get_data()))
        self$add_js_queue(cmd)
      }
      
      ## load data
      self$store$load_data()
    },
    transport_fun = function() {
      ## transport back row. Fine for multiple or not. Use id here, as sorting can
      ## otherwise mess up relationship between index and data in R data frame
##      "var param={value: Ext.pluck(this.getSelectionModel().getSelection(),'id')}"
      "var param={value: selected.map(function(rec) {return(rec.get('row_id'))})}"
      
    },
    process_transport = function(value, ...) {
      if(is.list(value))
        self$value <- sort(unlist(value))
      else
        self$value <- sort(value)
    },
    param_defn = function(signal) {
      if(signal == self$change_signal) {
        self$transport_fun()
      } else if(signal == "cellclick" ||
                signal == "celldblclick") {
        "param={row_index:rec.get('row_id'), column_index:cellIndex + 1};"
      } else if(signal == "headerclick" ||
                signal == "headerdblclick") {
        "param = {column_index:columnIndex + 1};"
      } else if(signal == "itemclick" ||
                signal == "itemdblclick") {
        "param = {value:rec.get('row_id')};"
      }
      else {
        "param=null;"
      }
    },
    
    get_value = function(index=FALSE, drop=TRUE, ...) {
      "Return selected value(s)"

      if(length(self$value) == 1 && is.na(self$value))
        return(NA)

      items <- self$store$get_data()

      if(drop)
        items[value, chosencol, drop=TRUE]
      else
        items[value, , drop=FALSE]
    },
    get_index=function(...) {
      if(length(self$value) == 1 && is.na(self$value))
        return(NA)
      else
        return(self$value)
    },
    set_value = function(value, index=TRUE, ...) {
      ## Value is index if numeric and index is TRUE
      ## value is logical if index is trye and logical
      ## value is matched against names
      if(is.logical(value)) {
        self$value <- which(value)
      } else {
        self$value <- match(value, self$get_items(j = self$chosencol))
      }
      self$set_index(value)
    },
    clear_selection=function() {
      cmd <- sprintf("%s.getSelectionModel().deselectAll()", self$get_id())
      self$add_js_queue(cmd)
    },
    set_index=function(value, ...) {
      "Set value where value is row index to select"
      if(is.logical(value)) {
        self$value <- which(value)
      } else {
        self$value <- value
      }
      self$clear_selection()
      if(base:::length(value) == 0 ||
         (base:::length(value) == 1 && is.na(value)) ||
         value[1] <= 0) {
        ## nothing
      } else {
        tpl <- "
{{id}}.getSelectionModel().selectRange({{start}},{{end}}, true);
"
        f <- function(start, end) {
           cmd <- whisker.render(tpl, list(id = self$get_id(),
                                           start = start-1, 
                                           end = end-1))
           self$add_js_queue(cmd)
        }
        ## should figure out runs to shorten this
        sapply(value, function(i) f(i,i))
      }
    },
    set_items = function(value, i, j, ...) {
      super$set_times(value, i, j, ...)

      cmd <- paste(sprintf("%s.getTotalCount = function() {return %s}",
                           self$store$get_id(), nrow(self$store$get_data())),
                   sep="")
      self$add_js_queue(cmd)
      
      if(self$paging) {
        ## need to notify grid that the total
        ## count has increased or decreased. This
        ## is done through the getTotalCount JS
        ## function
      } else {
        ## cmd <- sprintf("%s.getUpdater().update(%s)",
        ##                get_id(),
        ##                toJSObject(store$proxy$get_url_list())
        ##                )
        ## cmd <- sprintf("%s.doRequest(%s_);",
        ##                store$get_id(),
        ##                toJSObject(store$proxy$get_url_list()))
        ## add_js_queue(cmd)
      }
    },
    filter = function(colname, regex) {
      "Use filter by regular expression. No visible<- method to adjust visible rows"
      if(missing(colname) || is.na(match(colname, names(self$store$get_data()))))
        return()

      if(missing(regex) || nchar(regex) == 0) {
        cmd <- sprintf("%s.clearFilter();", self$store$get_id())
      } else {
        cmd <- sprintf("%s.filter(%s, RegExp('%s'));",
                       self$store$get_id(),
                       escapeSingleQuote(colname),
                       regex)
      }
      self$add_js_queue(cmd)
    },
    get_visible=function(...) {
      self$store$get_visible(...)
    },
    set_visible=function(value, ...) {
      self$store$set_visible(value, ...)
    }
  )
)
