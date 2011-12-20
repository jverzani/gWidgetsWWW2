##' @include array.R
NULL

## Misc files for ext

##' List containing callback argument for ExtJs Events
.ext_callback_arguments <- list(afteredit = "e",                 # for gdf cell editing
                                beforechange="tb, params",       # for paging toolbar
                                blur="w",                        # w = "this"
                                bodyresize = "w, width, height",
                                bodyscroll = "scrollLeft, scrollRight",
                                cellcontextmenu = "w, rowIndex, cellIndex, e",
                                cellclick = "w, rowIndex, columnIndex, e", # grid
                                celldblclick = "w, rowIndex, columnIndex, e", # grid
                                cellmousedown = "w, rowIndex, columnIndex, e", # grid
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
                                select = "w,record,index", beforeselect = "w, record, index",
                                selectionchange = "selModel, selected, opts",    # gcheckboxgrouptable
                                show = "w", beforeshow = "w", 
                                specialkey = "w, e",
                                tabchange = "w, tab", # notebook
                                toggle = "w, value",             # gtogglebutton
                                valid = "w")


##' a class to handle map between R and arguments for constructor
ExtArgs <- setRefClass("ExtArgs",
                       fields=list(
                         "args"="Array"
                         ),
                       methods=list(
                         initialize = function(...) {
                           args <<- Array$new(...)
                           callSuper()
                         },
                         extend = function(l, overwrite=TRUE) {
                         "Extend argument list by list l."
                         for(i in names(l)) {
                           if(!(i %in% names(args) ) ||
                              overwrite)
                             args$push(l[[i]], i)
                         }
                       },
                         to_js_object = function() {
                           "Convert to string of object literal"
                           toJSObject(args$core())
                         }
                         )
                       )
