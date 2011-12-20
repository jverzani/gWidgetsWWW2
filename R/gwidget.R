##' @include gcomponent.R
NULL

##' GWidget is the Base class for widget objects
GWidget <- setRefClass("GWidget",
                       contains="GComponent",
                       fields=list(
                         coerce_with="ANY" # function
                         ),
                       methods=list(
                         initialize=function(..., coerce.with) {
                           coerce_with <<- NULL
                           if(!missing(coerce.with)) {
                             if(is.character(coerce.with))
                               coerce_with <<- get(coerce.with, inherits=TRUE)
                             else
                               coerce_with <<- coerce.with
                           }

                           initFields(default_fill=FALSE,
                                      default_expand=FALSE)
                           
                           callSuper(...)
                         }
                         )
                       )
                       
                       ## Needs subclasses:
                       ## GWidgetWithItems (gradio, ...)
## GWidgetWithProxy (ghtml, gtable, gdf, ...)
## GWidgetText      (gtext)
