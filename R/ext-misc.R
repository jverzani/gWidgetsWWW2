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
NULL

## Misc files for ext

## List containing callback argument for ExtJs Events
.ext_callback_arguments <- list(afteredit = "e",                 # for gdf cell editing
                                beforechange="tb, params",       # for paging toolbar
                                blur="w",                        # w = "this"
                                bodyresize = "w, width, height",
                                bodyscroll = "scrollLeft, scrollRight",
                                cellcontextmenu = "w, rowIndex, cellIndex, e",
                                cellclick = "w, td, cellIndex, rec, tr, rowIndex, e, opts", # grid
                                celldblclick = "w, td, cellIndex, rec, tr, rowIndex, e, opts", # grid
                                change="w, newValue, oldValue, eOpts",
                                beforechange = "w, newValue, oldValue",
                                check = "w, checked",
                                click = "w, e", # gtree?
                                collapse = "w",                  # combobox
                                columnmove = "oldIndex, newIndex",
                                columnresize = "columnIndex, newSize",
                                dblclick = "e",                  # grid -- not celldblclick
                                destroy="w", beforedestroy = "w",
                                destroyed = "w, c", # child component destroyed
                                disable="w",
                                drag = "w, e", dragend = "w,e", dragstart = "w,e",
                                enable = "w",
                                expand = "w",                    # combobox
                                fileselected = "w, s",               # FileUploadField
                                focus = "w",
                                headerclick = "w, columnIndex, e", # grid
                                headercontextmenu = "w, columnIndex, e", # grid
                                headerdblclick = "w, columnIndex, e", # grid
                                headermousedown = "w, columnIndex, e", # grid       
                                hide = "w", beforehide = "w",
                                invalid = "w",
                                itemclick="view, rec, item, index, event, opts",
                                itemdblclick="view, rec, item, index, event, opts",
                                keydown = "w,e",                 # e Ext.EventObject
                                keypress = "w,e",
                                keyup = "w,e",
                                mousedown = "e",
                                mouseover = "e", 
                                mousemove = "e", 
                                move = "w, x, y",
                                render = "w", beforerender = "w",
                                resize = "w, adjWidth, adjHeight, rawWidth, rawHeight",
                                rowclick = "w, rowIndex, e", # grid
                                rowcontextmenu = "w, rowIndex, e", # grid
                                rowdblclick = "w, rowIndex, e", # grid
                                rowmousedown = "w, rowIndex, e", # grid       
                                select = "selModel,record,index,opts",
                                beforeselect = "selModel, record, index",
                                selectionchange = "selModel, selected, opts",    # gcheckboxgrouptable, gtable
                                show = "w", beforeshow = "w", 
                                specialkey = "w, e",
                                tabchange = "w, tab", # notebook
                                toggle = "w, value",             # gtogglebutton
                                valid = "w")


##' a class to handle map between R and arguments for constructor
##'
##' The ExtArgs are simply an Array instance wrapper with an extend method and
##' a conversion method
##' @exportClass ExtArgs
##' @name ExtArgs-class
ExtArgs <- setRefClass("ExtArgs",
                       fields=list(
                         "args"="Array"
                         ),
                       methods=list(
                         initialize = function(...) {
                           args <<- gWidgetsWWW2:::Array$new(...)
                           callSuper()
                         },
                         extend = function(l, overwrite=TRUE) {
                         "Extend argument list by list l."
                         QT <- sapply(names(l), function(i) {
                           if((is.logical(overwrite) && overwrite) ||
                              !(i %in% names(args) ))
                             args$push(l[[i]], i)
                         })
                       },
                         to_js_object = function() {
                           "Convert to string of object literal"
                           toJSObject(args$core())
                         }
                         )
                       )
