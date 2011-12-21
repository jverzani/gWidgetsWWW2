##' @include gcomponent.R
NULL

GContainer <- setRefClass("GContainer",
                          contains="GComponent",
                          fields=list(
                            "children"="Array",
                            spacing="numeric"
                            ),
                          methods=list(
                            initialize=function(...) {
                              initFields(
                                         children=Array$new(),
                                         spacing=5,
                                         default_fill=TRUE,
                                         default_expand=TRUE
                                         )
                              

                              callSuper(...)
                            },
                            child_bookkeeping=function(child) {
                              "Update parent property of child and children property of parent container"
                              if(is(child, "GComponent"))
                                child$set_parent(.self)
                              children$push(child, child$get_id())
                            },
                            add = function(child, expand, anchor, fill, ...) {
                              ## For compatibility with previous
                              ## we remove expand, anchor, fill as these are done through add_dots.
                              add_child(child,  ...)
                            },
                            add_child = function(child,  ...) {
                              "Add child to parent, do internal book keeping"
                              child_bookkeeping(child)
                              call_Ext("add", String(child$get_id())) # add to GUI
                              do_layout()
                            },
                            do_layout=function() {
                              call_Ext("doLayout") # call update method if needed
                            },
                            mapAnchorToCSSClass = function(anchor) {
                              "Return a css class for the anchor value"
                              if(is.null(anchor))
                                return("td-northwest")
                              if(all(anchor == 0))
                                return("td-center")
                              
                              lr <- c("west", "", "east")
                              ns <- c("north", "", "south")
                              m <- rbind(paste("td", "-", ns[1], lr, sep=""),
                                         paste("td", "-", ns[2], lr, sep=""),
                                         paste("td", "-", ns[3], lr, sep="")
                                         )
                              
                              m[ 2 - anchor[2], 2 + anchor[1]]
                            },

                            add_dots=function(child, expand=FALSE, fill=FALSE, anchor=NULL, ...) {
                              "Function to do process expand, fill, anchor arguments when adding a child"
                              ## This gets called before constructor is written, so we modify the args of the
                              ## child component
                              
                              
                              ## ## spacing first
                              ## if(is.numeric(spacing))
                              ##   child$add_args(list(style=list(padding=sprintf("%spx",spacing))))
                              ## else if(is.character(spacing) && nchar(spacing))
                              ##   ## eg spacing="'5px,0px,0px,5px'"
                              ##   child$add_args(list(style=list(padding=spacing)))
                              
                              ## expand, anchor fill
                              ## expand -- turned into flex value below
                              expand <- getWithDefault(expand, default=child$default_expand)
                              fill <- getWithDefault(fill, default=child$default_fill)
                              anchor <- getWithDefault(anchor, default=NULL)

                              ## turn expand into number 0=FALSE
                              ## this way flex can vary with default of TRUE=1
                              expand <- as.integer(expand)
                              if(expand)
                                child$add_args(list(flex=expand))

                              if(!is.null(fill)) {
                                fill <- switch(as.character(fill),
                                               "TRUE"="stretch",
                                               "both"="stretch",
                                               "x"=ifelse(has_slot("horizontal") &&  !horizontal, "stretch", "left"),
                                               "y"=ifelse(has_slot("horizontal") &&  horizontal, "stretch", "top"),
                                               fill)
                                ## add to already constructed container:
                                ## XXX fill=FALSE, glayout add_js_queue(sprintf("%s.layout.align='%s';", get_id(), fill))
                              }

                              
                              if((is.null(fill) || (is.logical(fill) && !fill)) &&
                                 (is.null(expand) || !expand)) {
                                if(!is.null(anchor))
                                  child$add_args(list(cls=mapAnchorToCSSClass(anchor)))
                              }
                              
                              ## ## fill. isn't working
                              ## fill <- getFromDots("fill", ..., default=NULL)
                              ## if(!is.null(fill)) {
                              ##   if(fill == "x")
                              ##     child$add_args(list(width="auto"))
                              ##   if(fill == "y")
                              ##     child$add_args(list(height="auto"))
                              ##   else
                              ##     child$add_args(list(width="auto", height="auto"))
                              ## }
                              
                              
                            },
                            set_child_align=function(child, alt_child, anchor) {
                            },
                            set_child_fill=function(child, fill, horizontal=TRUE) {
                              "Fill can be NULL, TRUE, FALSE, '', 'both', 'x', 'y'..."
                            },
                            delete=function(child, ...) {
                              "Remove child from container"
                              children$remove_by_name(child$get_id())
                              call_Ext("remove", String(child$get_id()))
                            },
                            do_layout=function() {
                              call_Ext("doLayout")
                            },
                            ## toolbar and statusbar things for panels. Inherited by ggroup, gwindow
                            docked_items=function() {
                              "Return string with docked items template"
                                                         tpl <- "[
 {
   xtype:'toolbar',
   dock:'top',
   id:'%s_toolbar'
 },
 {
   xtype:'toolbar',
   dock:'bottom',
   id:'%s_status_bar',
   items:[
          {
            xtype:'label',
            text:''
          }]
 }]
"
                           dockedItems <- String(sprintf(tpl, get_id(), get_id()))
                              dockedItems
                            },
                            status_id=function() sprintf("%s_status_bar", get_id()),
                            toolbar_id=function() sprintf("%s_toolbar", get_id()),
                            set_status = function(value="") {
                              "Set status bar text"
                              add_js_queue(sprintf("%s.getComponent(0).setText('%s');",
                                                   status_id(), value))
                              do_layout()
                            },
                            add_to_toolbar = function(lst) {
                              "Add a list of objects to the toolbar"
                              addToToolbar(lst, nm="", self=.self)
                            },
                            remove_from_toolbar=function(obj) {
                              "Remove object from toolbar"
                              add_js_queue(sprintf("%s.remove(%s)",
                                                   toolbar_id(), obj$get_id()))
                            },
                            ## gWidgets methods
                            set_enabled=function(value, ...) {
                              "Recursively enable/disable child components"
                              if(value)
                                call_Ext("cascade", String("function(){ this.enable()}"))
                              else
                                call_Ext("cascade", String("function(){ this.disable()}"))
                            },
                            set_height = function(px) {
                              "Set height in pixels"
                              call_Ext("setHeight", px)
                            },
                            set_width = function(px) {
                              "Set width in pixels"
                              call_Ext("setWidth", px)
                            },
                            set_size = function(value) {
                              "w is c(width, height)"
                              set_width(value[1])
                              if(length(value) > 1)
                                set_height(value[1])
                            },
                            set_visible = function(value) {
                              "Show container and its siblings"
                              if(value)
                                call_Ext("show")
                              else
                                call_Ext("hide")
                            }

                            ))

   
