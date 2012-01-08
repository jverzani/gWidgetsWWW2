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


##' A panel to hold other JavaScript things
##'
##' ##' @inheritParams gwidget
##' @return a \code{GPanel} reference class  object
##' @export
##' @examples
##' w <- gwindow("windows and dialogs")
##' g <- ggroup(cont=w, horizontal=FALSE)
##' 
##' pan <- gpanel(cont=g, width=200, height=200)
##' ## will load d3 javascript library
##' d3_url <- "http://mbostock.github.com/d3/d3.js?2.7.1"
##' ## template to simply insert HTML
##' ## div_id comes from pan$div_id()
##' tpl <- "
##' function(data, status, xhr) {
##'   d3.select('#{{div_id}}').html('Look ma HTML');
##' }
##' "
##' ## more complicated example to draw a line
##' tpl2 <- "
##' function(data, status, xhr) {
##' var chart = d3.select('#{{div_id}}').append('svg')
##'     .attr('class', 'chart')
##'     .attr('width', 200)
##'     .attr('height', 200);
##' 
##' chart.append('line')
##'     .attr('x1', 25)
##'     .attr('x2', 200 - 25)
##'     .attr('y1', 100)
##'     .attr('y2', 100)
##'     .style('stroke', '#000');
##' }
##' "
##' 
##' cmd <- whisker.render(tpl2, list(div_id=pan$div_id()))
##' pan$load_external(d3_url, cmd)

gpanel <- function(
                   container,
                   ...,
                   width=NULL,
                   height=NULL,
                   ext.args = NULL
                   ){
  g <- GPanel$new(container, ...)
  g$init(container, ..., width=width, height=height, ext.args=ext.args)
  g
}

GPanel <- setRefClass("GPanel",
                       contains="GWidget",
                       methods=list(
                         init = function(
                           container,
                           ...,
                           width =NULL,
                           height=NULL,
                           ext.args = NULL
                           ) {

                           constructor <<- "Ext.Panel"

                           arg_list <- list(border = TRUE,
                                            hideBorders = FALSE,
                                            width=width,
                                            height=height,
                                            html=sprintf("<div id='%s'></div>", div_id())
                                            )
                           

                           add_args(arg_list, ext.args)
                           write_constructor()
                           container$add(.self, ...)
                           
                         },
                         div_id = function() {
                           sprintf("%s_div", id)
                         },
                         load_external=function(url, callback) {
                           "Url is url of external script. Callback is a string representing a JavaScript function of three variables: data, status, xhr called if the download is successful. The request is asynchronous, so calling commands immediately after may not work,as the script needs to download first."
                           tpl <- "
$.ajax('{{url}}',{
    dataType: 'script',
    type:'GET',
    cache: true,
    success: {{fn}}
});
"
                           if(missing(callback))
                             callback <- "function(data, status, xhr) {}"

                           cmd <- whisker.render(tpl, list(url=url, fn=callback))
                           add_js_queue(cmd)
                         }
                         )
                       )

## This worked
## Ext.create('Ext.Button', {
##     text: 'Click me',
##     renderTo:Ext.get('gWidget_ID3_div'),
##     handler: function() {
##         alert('You clicked the button!')
##     }
## });
