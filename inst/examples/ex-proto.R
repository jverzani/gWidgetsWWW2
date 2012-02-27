## in rserve.R of FastRWeb, but here for local usage
require(proto); require(MASS); require(digest)

## Example of using a proto object. Not really needed here, but in general such programming
## styles can be quite useful and reference classes, which are the natural thing to use here,
## can give issues within this framework.

DFWidget <- proto(data_set=NULL,
                  data_set_name=NULL,
                  data_set_widget=NULL,
                  predictor="",
                  predictor_widget = "",
                  new = function(., ...) {
                    .$proto(...)
                  },
                  init_ui=function(., container) {
                    g <- gvbox(container=container)
                    glabel("Select a data frame:", cont=g)
                    .$data_set_widget <- gcombobox(c("mtcars", "Cars93", "Aids2"), cont=g,
                                                   handler=function(h,...) {
                                                     nm <- svalue(h$obj)
                                                     .$set_data(get(nm, .GlobalEnv), nm)
                                                   })
                    glabel("Select a variable:", cont=g)                    
                    .$predictor_widget <- gcombobox(c(""), cont=g,
                                                    handler=function(h,...) {
                                                      .$set_predictor(svalue(h$obj))
                                                    })
                    enabled(.$predictor_widget) <- FALSE
                    .$update_view()
                  },
                    
                  set_data=function(., value, nm=deparse(substitute(value))) {
                    old_value <- .$data_set
                    .$data_set <- value
                    .$data_set_name <- nm
                    .$predictor <- ""

                    if(is.null(old_value) || digest(old_value) != digest(value))
                      .$update_view()
                  },
                  set_predictor=function(., value) {
                    .$predictor <- value
                    message("set predictor")
                  },
                  update_view=function(.) {
                    message("update view")
                    if(is.null(.$data_set)) {
                      svalue(.$data_set_widget, index=TRUE) <- 0
                      
                      svalue(.$predictor_widget, index=TRUE) <- 0
                      .$predictor_widget[] <- c("")
                      enabled(.$predictor_widget) <- FALSE                      
                    } else {
                      svalue(.$data_set_widget) <- .$data_set_name

                      enabled(.$predictor_widget) <- TRUE                      
                      .$predictor_widget[] <- names(.$data_set)
                      svalue(.$predictor_widget) <- " "
                    }
                    if(.$predictor != "")
                      svalue(.$predictor_widget) <- .$predictor
                  }
                  )

w <- gwindow("some test")
g <- gframe("data selector", cont=w, horizontal=FALSE)
ghtml("
The use of reference class methods can make some tasks of GUI programming much easier. However these
can be problematic within a <b>gWidgetsWWW2</b> script. This example shows how to use the <b>proto</b>
package to achieve something similar. For server usage, the package is best loaded when <b>FastRWeb</b> starts, so that it is
a) loaded only once and b) available in the callbacks.
", cont=g)
a <- DFWidget$new()
a$init_ui(g)
