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
NA

##' Base class for text widgets
GWidgetText <- setRefClass("GWidgetText",
                           contains="GWidget",
                           methods=list(
                             initialize=function(...) {
                               initFields(change_signal="change")
                               callSuper(...)
                             },
                             set_value = function(value, ...) {
                               "set text"
                               value <<- value
                               call_Ext("setValue", value)
                             },
                             transport_fun = function() {
                               "param = {value: this.getValue()};"
                             },
                             param_defn = function(signal) {
                               if(signal == "keyup") {
                                 out <- "var param = {key:e.getKey()}"
                               } else {
                                 out <- ""
                               }
                               return(out)
                             }
                             ))
