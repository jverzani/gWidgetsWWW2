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

## In Ext4 the MVC pattern is used throughout. A widget has a backend
## store associated to it from where it gets its data. This store has
## either an implicit or explicit model. As well, the store can store
## its data locally or have the data come via a proxy, in either case
## the store needs a reader to read the data coming from the proxy.
##
## In gWidgetsWWW2 we follow suit creating classes for both proxies
## and stores. The store writes the model and specifies a reader, as
## well it specifies an underlying proxy object.
##
## Stores need methods for a) returning data from a request, b)
## displaying javascript to make request
## 
## The numbering of objects is roughly:
## ogWidget_id1 = proxy(url)
## ogWidget_id(1+1) = store
## ogWidgets_id(1+2) = GridPanel
## 
## The proxy returns the values to the Ext store in response to an
## AJAX request. The call_proxy routine in gwidgets-toplevel calls
## the get_json_data method of the proxy. Proxies typically return
## JSON encoded data, though it may also be desirable to return other
## values. (In those cases, we still call get_json_data and
## deliberately avoid the name
## implication.)


##' interface between R objects and JSON to supply Ext objects with data
GWidgetProxy <- setRefClass("GWidgetProxy",
                        contains="GWidget",
                        fields=list(
                          value="ANY"
                          ),
                        methods=list(
                          init=function(value, ...) {
                            
                            value <<- value
                            constructor <<- "Ext.data.proxy.Ajax"
                            arg_list <- merge.list(
                                                   get_url_list(),
                                                   list(
                                                        method="GET",
                                                        reader="array"
                                                        ))
                            add_args(arg_list)

                            write_constructor()
                          },
                          get_url_list = function() {
                            "Get url to call back into this proxy object. The values 'proxy_url' and 'session_id' are JS globals in webpage"
                            list(url=String("proxy_url"),
                                 params=list(
                                   session_id=String("session_id")
                                   )
                                 )
                          },
                          get_json_data=function(...) {
                            "The params determine what to pass back. "
                            ## Default is just to return array of objects
                            if(is.data.frame(value)) {
                              out <- sapply(seq_len(nrow(value)), function(i) toJSObject(value[i,]))
                              ret <- sprintf("[%s]", paste(out, collapse=","))
                            }

                            return(ret)
                          },
                          get_data=function(...) {
                            "Return raw data, not JSON encoded"
                            value
                          },
                          set_value = function(value, ...) {
                            "Set data and call load function"
                            value <<- value
                            ## Call load function goes here ...
                          },
                          set_data=function(value, ...) {
                            "Set proxy data"
                            value <<- value
                          },
                          get_fields=function() {
                           "Return json with fields mapped to names"
                           ""
                          }
                          ))

##' Proxy for ghtml
GWidgetHTMLProxy <- setRefClass("GWidgetHTMLProxy",
                            contains="GWidgetProxy",
                            methods=list(
                              get_json_data=function(...) {
                                "Despite the name, return raw data, not JSON encoded"
                                value
                              }
                              ))


## For make_column_model we need to specify various things: renderer,
## xtype, and editor.
##
## These S3 generics are used to do so based on the class of the
## column


## this function is used internally by make_column_model but for some
## reason doesn't work when defined within that method body the
## rendererers are defined in gw-gtable.js

##' Generic function to render a column in ExtJS Grid
##' @param x object to get column class from
column_renderer <- function(x) UseMethod("column_renderer")

##' Method to render a column in ExtJS Grid
##' @param x object to get column class from
column_renderer.default <- function(x) list(sortable=TRUE)

##' Method to render a column in ExtJS Grid
##' @param x object to get column class from
column_renderer.Icon    <- function(x) list(width=20, sortable=FALSE, renderer=String("gtableIcon"))


## This function is use by make_column_model when editing of cells is
## desired. They return a configuration list

##' Generic function to create column editor for a table widget
##' @param x type of object
##' @param ... ignored
column_editor <- function(x, ...) UseMethod("column_editor")

column_editor.character <- function(x, ...) {
  list(editor="textfield")
}

##'  Method to create column editor for a table widget
##' @param x type of object
##' @param ... ignored
column_editor.default <- function(x, ...)  {
  list(editor=list(xtype="textfield"))
  ##list(editor = String("new Ext.form.TextField"))
}

##' Method to create column editor for a table widget
##' @param x type of object
##' @param ... ignored
column_editor.integer <- function(x, ...) {
  list(editor=list(xtype="numberfield", format="0", hideTrigger=TRUE,  nanText="NaN"))
#  list(editor =  String(sprintf("new Ext.form.NumberField(%s)",
#         toJSObject(list(allowBlank=TRUE, allowDecimals=FALSE, nanText='NA')))))
}

##' Method to create column editor for a table widget
##' @param x type of object
##' @param ... ignored
column_editor.numeric <- function(x, ...) {
    list(editor=list(xtype="numberfield", hideTrigger=TRUE, decimalPrecision=16L, nanText="NaN"))
}

## should be Checkbox, but can't get to work

##' Method to create column editor for a table widget
##' @param x type of object
##' @param ... ignored
column_editor.logical <- function(x, ...) {
  list(editor=list(xtype="checkbox",cls="x-grid-checkheader-editor"))
  ## list(editor =  String(sprintf("new Ext.form.ComboBox(%s)",
  ##        toJSObject(list(typeAhead = TRUE,
  ##                        editable=FALSE,
  ##                        triggerAction = "all",
  ##                        store = String("['true', 'false']"),
  ##                        lazyRender=TRUE,
  ##                        listClass="x-combo-list-small"
  ##                        )))))
}
 


##' Method to create column editor for a table widget
##' @param x type of object
##' @param ... ignored
column_editor.factor <- function(x, ...) {
#  list(editor=list(xtype="textfield"))
  list(editor=list(
         xtype="combobox",
         typeAhead=TRUE,
         triggerAction="all",
         selectOnTab=TRUE,
         store=String(
           paste("[",
                 paste(sapply(sort(levels(x)), function(i) toJSArray(rep(i,2))), collapse=","),
                 "]", sep="")),
         lazyRender=TRUE,
         listClass='x-combo-list-small'
         ))
  ## list(editor =  String(sprintf("new Ext.form.ComboBox(%s)",
  ##        toJSObject(list(typeAhead = TRUE,
  ##                        editable=FALSE,
  ##                        triggerAction = "all",
  ##                        store = String(sprintf("[%s]", paste(sapply(levels(x), ourQuote),collapse=","))),
  ##                        lazyRender=qTRUE,
  ##                        listClass="x-combo-list-small"
  ##                        )))))
}
column_editor.Date <- function(x, ...) {
  list(editor="datefield", format= 'Y/m/d')
}


## What xtype?
column_xtype <- function(x, ...) UseMethod("column_xtype")
column_xtype.default <- function(x, ...) {
  list()
}

column_xtype.integer <- function(x, ...) {
  list(xtype="numbercolumn", format="0")
}

column_xtype.numeric <- function(x, ...) {
  list(xtype="numbercolumn", format="0.000,00/i")
}

column_xtype.logical <- function(x, ...) {
 list(xtype="booleancolumn"
      ,trueText= 'TRUE'
      ,falseText= 'FALSE'
      )
}

column_xtype.Date <- function(x, ...) {
  list(xtype="datecolumn", format= 'Y/m/d')
}



##' Proxy to return array data (combobox, table, gdf, ...) [[],[],[]]
GWidgetArrayProxy <- setRefClass("GWidgetArrayProxy",
                             contains="GWidgetProxy",
                             fields=list(
                               col.widths="ANY",
                               update_url_list = "list",
                               ..visible="logical"
                               ),
                             methods=list(
                                init=function(value, ...) {
                                  
                                  value <<- value
                                  ..visible <<- rep(TRUE, nrow(value))
                                  col.widths <<- getFromDots("col.widths", ..., default=NULL)

                                  ## Would like to use Ext.data.proxy.Rest here, but
                                  ## the PUT requests get blocked by Rook. Not sure why
                                  ## Using Rest would allow us to handle put/create/delete requests.
                                  ## CUrrently we can'st do this, so we have no way to delete a record
                                  ##constructor <<- "Ext.data.proxy.Rest"
                                  constructor <<- "Ext.data.proxy.Ajax" 
                                  arg_list <- get_url_list()
                                  arg_list <- merge.list(arg_list,
                                                         list(method="GET",
                                                              reader="array",
                                                              extraParams=list(
                                                                session_id=String("session_id"),
                                                                id=get_id(),
                                                                total=nrow(value)
                                                                )
                                                              ))
                                  add_args(arg_list)
                                  add_public_method(c("add_row", "remove_row"))
                                  write_constructor()
                                },
                               get_data=function(drop_visible=TRUE,...) {
                                 "Return raw data, not JSON encoded"
                                 DF <- value
                                 if(drop_visible)
                                   DF[..visible,, drop=FALSE] ## want a data frame!
                                 else
                                   DF
                               },
                               set_data = function(value, i, j, ...) {
                                 if(missing(i) && missing(j))
                                   value <<- as.data.frame(value, stringsAsFactors=FALSE)
                                 else
                                   value[i,] <<- value
                                 ..visible <<- rep(TRUE, nrow(value))

                                 
                               },
                               get_json_data=function(...) {
                                 "Return JSON array [[],[],] ... *or* handle post data!"

                                 
                                 params <- list(...)
                                 
                                 df <- cbind("row_id"=seq_len(nrow(.self$value)),
                                             .self$value)
                                 df <- df[..visible, ,drop=FALSE]


                                 ## do we have paging type request? We do if params$start is not null

                                 
                                 if(!is.null(params$start)) {
                                   start <- as.numeric(params$start) + 1
                                   limit <- as.numeric(params$limit)
                                   m <- nrow(df)

                                   ind <- seq_len(m)
                                   if(m > 0 && m >= start) {
                                     ind <- seq(start, min(m, start+limit))
                                   }
                                   ## Now we may be sorting, in which case we
                                   ## use apply the indices to the ordered values
                                   if(!is.null(params$sort)) {
                                     ## make a list
                                     sort_info <- as.list(unlist(fromJSON(params$sort)))
                                     direction <- c(ASC=FALSE, DESC=TRUE)

                                     x <- df[, sort_info$property]
                                     ordered <- order(x,
                                                      decreasing=if(sort_info$direction == "ASC") FALSE else TRUE
                                                      )

                                     ind <- ordered[ind]

                                   }  

                                   df <- df[ind,,drop=FALSE]
                                   
                                 }
### XXX work with params
                                 ## gsub("\\n", "",sprintf("[%s]",paste(apply(df, 1, toJSON),collapse=",")))
                                 toJSArray(df)
                               },
                               post_json_data=function(param) {
                                 "A post request from updating a store"

                                 l <- param
                                 ## in form l$row_id, and rest

                                 ## JV: check where id is! this makes a big deal whether we are
                                 ## inserting or replacing data. Nothing else passed in as far as I can tell
                                 
                                 multi <- is.null(l$row_id)
                                 if(multi) {
                                   ind <- sapply(l, function(i) {
                                     value[i$row_id, ] <<- lapply(i[-1], function(j) ifelse(is.null(j), NA, j))
                                     i$id
                                   })
                                 } else {
                                   ## if id is 0 we are inserting at end
                                   ## sychronize with javascript code in gdf
                                   if(l$row_id==0) {
                                     ind <- nrow(value) + 1
                                   } else {
                                     ind <- l$row_id
                                   }
                                   l$id <- NULL # don't insert this
                                   value[ind,] <<- lapply(l[-1], function(i) ifelse(is.null(i), NA, i))


                                 }
                                 ## Return value for record, incase we want to make local changes to push
                                 ## back to client
                                 vals <- seq_len(nrow(value))
                                 df <- cbind("row_id"=vals[ind], value[ind,])
                                 String(toJSArray(df))
                               },
                               add_row=function(row, ...) {
                                 value <<- rbind(value, rep(NA, ncol(value))) # add new?
                                 ##   value[unlist(row),] <<- rep(NA, ncol(value)) # add new?
                                 ..visible[unlist(row)] <<- TRUE
                               },
                               remove_row=function(param) {
                                 "Remove the row"
                                 if(nrow(value) > 1) {
                                   row <- as.numeric(unlist(param))
                                   value <<- value[-row,, drop=FALSE]
                                   ..visible <<- ..visible[-row]
                                 } else {
                                   ## can't remove last row?
                                 }
                               },
                               get_visible=function(...) {
                                 ..visible
                               },
                               set_visible=function(val,...) {
                                 if(length(val) == nrow(value))
                                   ..visible <<- as.logical(val)
                               },
                               get_fields=function() {
                                 "Return fields mapping from name to type"
                                 if(nrow(value)) {
                                   df <- cbind("row_id"=seq_len(nrow(value)), value)
                                   makeFields(df)
                                 } else {
                                   ""
                                 }
                               },
                               
                               make_column_model = function(do.editor=FALSE) {
                                 ## return array for columns
                                 ## id, header, sortable, renderer, dataIndex, tooltip
                                 ##     columns: [
                                 ##               {id:'company',header: "Company", sortable: true, dataIndex: 'company'},
                                 ##               {header: "Price",  sortable: true, renderer: 'usMoney', dataIndex: 'price'},
                                 ##               {header: "Change", sortable: true, renderer: change, dataIndex: 'change'},
                                 ##               {header: "% Change", sortable: true, renderer: pctChange, dataIndex: 'pctChange'},
                                 ##               {header: "Last Updated", sortable: true, renderer: Ext.util.Format.dateRenderer('m/d/Y'), dataIndex: 'lastChange'}
                                 ##               ],

                                
                                 
                                 items <- value
                                 n <- ncol(items)
                                 
                                 ## names
                                 colIDs <- names(items)
                                 colNames <- colIDs

                                 ## adjust IDs
                                 colIDs <- gsub("[ \\.\\,] ","_",colIDs)

                                 ## adjust for icons
                                 ind <- sapply(items, isIcon)
                                 if(any(ind)) {
                                   for(i in which(ind)) {
                                     ## uggh
                                     tmp <- sapply(items[,i], getStockIconByName, css=FALSE)
                                     tmp[sapply(tmp, is.null)] <- ""
                                     items[i] <- asIcon(unlist(tmp))
                                     colNames[i] <- NA
                                   }
                                   value <<- items # update                                   
                                 }


                                 sortable <- rep(TRUE, length.out=n)
                                 if(is.null(col.widths))
                                   col.widths <<- rep(NA, length.out=n)

                                 res <- sapply(seq_len(n), function(i) {
                                   l <- list(#id=colIDs[i],
                                             header=colNames[i],
#                                             sortable=TRUE,
                                             dataIndex = colIDs[i]
                                             )
                                   if(i == n)
                                     l$flex <- 1
                                   else
                                     l$width <- col.widths[i] 
                                   l <- merge(l, column_xtype(items[,i]))

                                   if(do.editor)
                                     l <- merge(l, column_editor(items[,i]))
                                   else
                                     l <- merge(l, column_renderer(items[,i]))
                                   toJSObject(l)
                                 })
                                 
                                 out <- sprintf('[%s]', paste(res,collapse=","))
                                 return(out)
                               }
                               
                               ))



## tree proxy is all we need for gtree
GWidgetTreeProxy <- setRefClass("GWidgetTreeProxy",
                            contains="GWidgetArrayProxy",
                            fields=list(
                              offspring="function",
                              offspring.data="ANY"
                              ),
                            methods=list(
                              init=function(offspring, offspring.data=NULL) {
                                offspring <<- offspring
                                offspring.data <<- offspring.data

                                ## is there a constructor?
                                constructor <<- "Ext.data.proxy.Ajax" 
                                arg_list <-  list(url=String("proxy_url"),
                                                  method="GET",
                                                  extraParams=list(
                                                    session_id=String("session_id"),
                                                    id=get_id()
                                                    ),
                                                  reader=list(
                                                    type="json",
                                                    root="children"
                                                    )
                                                  )
                                add_args(arg_list)
                              },
                              set_data = function(value) {
                                "Set data into proxy"
                              },
                              get_json_data=function(node, sort, ...) {
                                if(is.null(node)) {
                                  ## Something wierd here
                                } else {
                                  path <- strsplit(node,":")[[1]][-1]
                                  children <- call_offspring(node, path)
                                  ## create row by row
                                  res <- sapply(seq_len(nrow(children)), function(i) toJSObject(children[i,]))
                                  out <- sprintf("[%s]", paste(res, collapse=","))
                                }
                                out
                              },
                              call_offspring=function(node, path) {
                                "Create children from path"
                                children <- offspring(path, offspring.data)
                                ## we create data frame with id, offspring, icons, then rest
                                
                                ## we expect key, offspring, icons, others. A column named qtip is special
                                if(ncol(children) == 1)
                                  children[,2] <- rep(FALSE, nrow(children))
                                if(ncol(children) == 2)
                                  children[,3] <- rep("", nrow(children))                         
                                children[3] <- sapply(children[3], getStockIconByName, css=TRUE)
                                
                                ## we make first 3 id, leaf, iconCls and make sure id not otherwised used
                                names(children)[2:3] <- c("leaf","iconCls") # mean something to extjs
                                if(names(children)[1] == "row_id") names(children)[1] <- "ID"
                                names(children)[names(children) == "row_id"] <- sapply(seq_len(sum(names(children)=="row_id")), function(i) sprintf("id_%s",i))
                                
                                ids <- sprintf("%s:%s", node, gsub(":","_",children[,1]))
                                ochildren <- cbind(id=ids, children[2], children[3], children[1], children[-(1:3)])
                                names(ochildren) <- gsub("\\.", "_",names(ochildren))
                                ochildren
                              },
                              make_columns=function() {
                                "Make columns specification for tree"
                                ## called in gtree constructor
                                df <- call_offspring("",character(0))
                                df <- df[,-(1:3)] # drop first 3
                                nms <- names(df)
                                nms <- setdiff(nms, "qtip")
                                out <- c(sprintf("{xtype:'treecolumn', flex:2, text:'%s', dataIndex:'%s'}",
                                                 nms[1], nms[1]),
                                         sapply(nms[-1], function(nm) {
                                           sprintf("{dataIndex:'%s', text:'%s', flex:1}", nm, nm)
                                         }))
                                String(sprintf("[%s]", paste(out, collapse=",")))
                              },
                              make_fields=function() {
                                ## used to define model
                                df <- call_offspring("",character(0))
                                makeFields(df[,-(1:3), drop=FALSE])
                              }
                            ))





## Helper Functions to makeFields.

##' Map a type
##'
##' @param x object
##' @export
mapTypes <- function(x) UseMethod("mapTypes")

##' mapTypes method
##' 
##' @method mapTypes default
##' @S3method mapTypes default
##' @rdname mapTypes
mapTypes.default <- function(x) list()


##' mapTypes method
##' 
##' @method mapTypes String
##' @S3method mapTypes String
##' @rdname mapTypes
mapTypes.String <- function(x) list(type="string")

##' mapTypes method
##' 
##' @method mapTypes integer
##' @S3method mapTypes integer
##' @rdname mapTypes
mapTypes.integer <- function(x) list(type="int")

##' mapTypes method
##' 
##' @method mapTypes numeric
##' @S3method mapTypes numeric
##' @rdname mapTypes
mapTypes.numeric <- function(x) list(type="numeric")

##' mapTypes method
##' 
##' @method mapTypes logical
##' @S3method mapTypes logical
##' @rdname mapTypes
mapTypes.logical <- function(x) list(type='bool')

##' mapTypes method
##' 
##' @method mapTypes factor
##' @S3method mapTypes factor
##' @rdname mapTypes
mapTypes.factor <- function(x) list()

##' mapTypes method
##' 
##' @method mapTypes date
##' @S3method mapTypes date
##' @rdname mapTypes
mapTypes.date <- function(x) list()

##' helper function to write field names for Ext constructor
##' @param df data frame
makeFields <- function(df) {
  ##
  ## return something like this with name, type
  ##     fields: [
  ##            {name: 'company'},
  ##            {name: 'price', type: 'float'},
  ##            {name: 'change', type: 'float'},
  ##            {name: 'pctChange', type: 'float'},
  ##            {name: 'lastChange', type: 'date', dateFormat: 'n/j h:ia'}
  ##         ]
  ## types in DataField.js
  ## if(!is.na(ind <- match(".id", names(df))))
  ##   df <- df[, -ind, drop=FALSE]
    
  
  types <- sapply(df, mapTypes)
  colNames <- names(df)
  res <- sapply(seq_len(ncol(df)), function(i) {
    l <- list(name=colNames[i])
    l <- merge.list(l, types[[i]])
    toJSObject(l)
  })
  out <- sprintf("[%s]", paste(res, collapse=","))

  return(out)
}


##################################################
## Stores
##
                            
##' Basic store, no default proxy so this isn't useful without being subclassed
GWidgetStore <- setRefClass("GWidgetStore",
                        contains="GWidget",
                        fields=list(
                          proxy="ANY"
                          ),
                        methods=list(
                          init = function(value, ...) {
                            "Initialize proxy, then set up store"
                            proxy <<- GWidgetProxy$new(toplevel)
                            proxy$init(value, ...)
                            ## ? need to write out store code here...

                            
                          },

                          get_data = function(...) {
                            "Get data in store"
                            proxy$get_data(...)
                          },
                          set_data = function(value, ...) {
                            "Set data for store"
                            proxy$set_data(value, ...)
                          },
                          model_id=function() {
                            sprintf("%s_model", get_id())
                          },
                          write_model=function() {
                            "Write out model"
                            cmd <- sprintf("Ext.define('%s',{extend: Ext.data.Model,fields:%s});",
                                           model_id(), proxy$get_fields())
                            add_js_queue(cmd)
                          }
                          )
                        )

##' A store for an array (inbetween widget and proxy)
GWidgetArrayStore <- setRefClass("GWidgetArrayStore",
                         contains="GWidgetStore",
                         fields=list(
                           page_size = "numeric",
                           paging = "logical"
                           ),
                         methods=list(
                           init = function(df, paging=FALSE, page.size=200, extra_args=list(),  ...) {
                             if(missing(df))
                               df <- data.frame(name=character(0), stringsAsFactors=FALSE)

                             paging <<- as.logical(paging)
                             page_size <<- as.integer(page.size)
                             proxy <<- GWidgetArrayProxy$new(toplevel)
                             col.widths <- getFromDots("col.widths", ..., default=NULL)
                             proxy$init(df, col.widths)
                             

                             constructor <<- "Ext.data.ArrayStore"
                             arg_list <- list(
                                              pageSize=page_size
                                              ,autoLoad=FALSE
                                              ,storeId=get_id()
                                              ,idIndex= 0
                                              ,proxy = String(.self$proxy$get_id())
                                              ##
                                              ,model=String(model_id())
##                                              ,buffered=TRUE
                                              ,autoSync=TRUE
                                              ,remoteSort=TRUE
                                           )

                             
                             
                             arg_list <- merge(arg_list, extra_args)
                             add_args(arg_list)

                             write_model()
                             write_constructor()

                             
#                             cmd <- paste(sprintf("%s.on('beforeload', function(store, options) {", get_id()),
#                                          sprintf("Ext.apply(options.params, {id:'%s', session_id:session_id});",
#                                                  proxy$get_id()),
#                                          "});",
#                                          sep="")
#                             add_js_queue(cmd)
                           },
                           write_model=function() {
                             "Write out model"
                             cmd <- sprintf("Ext.define('%s',{extend: Ext.data.Model,fields:%s});",
                                            model_id(), proxy$get_fields())
                             add_js_queue(cmd)
                           },
                           load_data=function(callback="function(){}") {
                             if(paging)
                               cmd <- sprintf("%s.load({params:{start:0, limit: %s, session_id:session_id, id:'%s'}, add:false, callback:%s});",
                                              get_id(), page_size, proxy$get_id(),callback)
                             else
                               cmd <- sprintf("%s.load({params:{session_id:session_id, id:'%s'}, add:false, callback:function() {}});",
                                              get_id(), proxy$get_id())
                             add_js_queue(cmd)
                           },
                           get_visible=function(...) {
                             proxy$get_visible(...)
                           },
                           set_visible=function(value, ...) {
                             proxy$set_visible(value, ...)
                             load_data()
                           }
                           )
                         )



GWidgetTreeStore <- setRefClass("GWidgetTreeStore",
                                contains="GWidgetStore",
                                fields=list(
                                  proxy="GWidgetTreeProxy"
                                  ),
                                methods=list(
                                  init=function(container, offspring, offspring.data, multiple) {
                                    initFields(
                                               constructor="Ext.data.TreeStore",
                                               proxy=GWidgetTreeProxy$new(container)
                                               )


                                    arg_list <- list(model=String(model_id()),
                                                     proxy=String(proxy$get_id()),
                                                     ## root=list(
                                                     ##   text="",
                                                     ##   id=sprintf("%s_root", get_id()),
                                                     ##   expanded=TRUE
                                                     ##   ),
                                                     folderSort=TRUE)
                                    add_args(arg_list)

                                    proxy$init(offspring, offspring.data)
                                    write_model()
                                    proxy$write_constructor()
                                    write_constructor()

                                  },
                                  write_model=function() {
                                    cmd <- paste(sprintf("Ext.define('%s',{", model_id()),
                                                 "extend: 'Ext.data.Model',",
                                                 sprintf("fields: %s", proxy$make_fields()),
                                                 "});",
                                                 sep="")
                                    add_js_queue(cmd)
                                  }

                                  ))
                                    
