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

##' Google maps widget
##'
##' Widget to display a google map and expose some of the google maps
##' API through R methods
##' @title Google maps widget
##' @param center lat/lng pair where map should be centered
##' @param zoom zoom level for initial map
##' @param maptype Type of map
##' @inheritParams gwidget
##' @return a \code{GGoogleMaps} object
##' @note The bulk of the functionality is provided through reference class methods.
##' @export
##' @examples
##' w <- gwindow("hello", renderTo="replaceme")
##' sb <- gstatusbar("Powered by gWidgetsWWW and Rook", cont=w)
##' 
##' ## Show map of central park
##' central_park <- c(40.782686, -74.01) ## central park
##' 
##' ## make map
##' gm <- ggooglemaps(cont=w, center=central_park, zoom=12, height=400)
##' 
##' ## Show how to click to add a polygon
##' g1 <- ggroup(cont=w)
##' glabel("Click some points, then click button to: ", cont=g1)
##' x <- numeric(0); y <- numeric(0)
##' b <- gbutton("add polygon", cont=g1, handler=function(h,...) {
##'   ggooglemaps_polygon(cbind(x, y), stroke=list(color="#FF00FF"), fill=list(color="#00FF00"), map=gm)
##' })
##' addHandlerClicked(gm, handler=function(h,...) {
##'   ggooglemaps_marker(position=c(h$lat, h$lng), title="", map=gm)
##'   x <<- c(x, h$lat); y <<- c(y, h$lng)
##' })
##' ## How to add a marker with a callback to open an info window
##' m <- ggooglemaps_marker(position=central_park, title="click me", map=gm)
##' addHandlerClicked(m, handler=function(h,...) {
##'   ggooglemaps_infowindow(h$obj, content = "Hello world", map=gm)
##' })
##' 
##' 
##' ## Buttons to toggle traffic and bicycle overlays
##' g1 <- ggroup(cont=w)
##' glabel("Change map type", cont=g1)
##' gcombobox(c("roadmap", "satellite", "hybrid", "terrain"), cont=g1, handler=function(h,...) {
##'   gm$set_maptype(svalue(h$obj, index=FALSE))
##' })
##' gcheckbox("Traffic overlay", checked=FALSE, width=150, cont=g1, handler=function(h,...) {
##'   gm$add_trafficlayer(svalue(h$obj))
##' })
##' gcheckbox("Bicycle overlay", checked=FALSE, width=150, cont=g1, handler=function(h,...) {
##'   gm$add_bikelayer(svalue(h$obj))
##' })
ggooglemaps <- function(center=c(45,45), zoom=13,
                        maptype="roadmap",
                        container, ...,
                        width=NULL, height=400, ext.args=NULL
                        ) {
  gm <- GGoogleMaps$new(container,...)
  gm$init(center, zoom, match.arg(maptype, c("roadmap", "satellite", "hybrid", "terrain")), container, ...,
          width=width, height=height, ext.args=ext.args)
  gm
}

##' Base class for GoogleMaps objects
##'
##' The \code{GGoogleMaps} class provides the methods for the
##' googlemaps widget. As the API for google maps far exceeds the
##' primitive API of \pkg{gWidgets}, reference class methods are used
##' to expose the google maps API. See the example for illustrations.
##' @rdname ggooglemaps
GGoogleMaps <- setRefClass("GGoogleMaps",
                           contains="GWidget",
                           fields=list(
                             stub="ANY"
                             ),
                           methods=list(
                             init = function(center,
                               zoom,
                               maptype,
                               container, ...,
                               width=NULL, height=NULL, ext.args=NULL) {

                               initFields(
                                          constructor="Ext.ux.GMapPanel"
                                          )

                               map_type <- c("roadmap"="map",
                                             "satellite"="G_SATELLITE_MAP",
                                             "hybrid"="G_HYBRID_MAP",
                                             "terrain"="G_PHYSICAL_MAP")

                               gmapType <- map_type[maptype]

                               print(list("googlemaps", maptype=maptype, gmapType=gmapType))
                               
                               arg_list <- list(
                                                setCenter=list(
                                                  lat=center[1],
                                                  lng=center[2]
                                                  ),
                                                #gmapType=gmapType,
                                                zoomLevel=zoom,
                                                width=width,
                                                height=height
                                                )

                               
                               add_args(arg_list)

                               setup(container, NULL, NULL, ext.args, ...)

                             },
                             ## helper functions
                             get_map = function() {
                               "Return jS call to get google map. No trailing ; here"
                               sprintf("%s.getMap()", get_id())
                             },
                             call_method = function(meth, ...) {
                               "Call method of map. Change arguments into object through coerceToJSString"
                               l <- list(...)
                               out <- sapply(l, coerceToJSString)
                               cmd <- sprintf("%s.%s(%s);",
                                              get_map(),
                                              meth,
                                              paste(out, collapse=","))
                               cmd
                               add_js_queue(cmd)
                             },
                             ## args = c("e","W") say
                             ## param = JavaScript code to define param value passed back to function
                             add_R_callback = function(signal, cb_args="", param_defn="var param=null") {
                               "Add handler, call on map, not Ext guy"
                               ## We need to buffer this, otherwise the handler call might not have a map to listen
                               ## to. Do do this we set up a delayed task.
                               
                               tpl <- "
var _addL = function() {
  google.maps.event.addListener({{map}}, '{{signal}}', function({{args}}) {
    {{param_defn}};
    callRhandler('{{id}}', '{{signal}}', param);
  })
}
"
                               cmd <- whisker.render(tpl, list(
                                                               map=get_map(),
                                                               signal=escapeSingleQuote(signal),
                                                               args=paste(cb_args,collapse=","),
                                                               param_defn=param_defn,
                                                               id=get_id()))
                               add_js_queue(cmd)
                               ## now call this function after a delay
                               cmd <- paste("new Ext.util.DelayedTask(_addL).delay(500);",
                                            sep="")
                               add_js_queue(cmd)
                             },
                             add_handler_changed = function(handler, action=NULL, ...) {
                               add_handler_clicked(handler, action, ...)
                             },
                             add_handler_clicked = function(handler, action=NULL, ...) {
                               cb_args <- "e"
                               param_defn <- "var param={lat: e.latLng.lat(), lng: e.latLng.lng(), x: e.point.x, y: e.point.y};"
                               add_handler("click", handler, action, cb_args=cb_args, param_defn=param_defn)
                             },
                             add_handler_mouse_motion = function(handler, action=NULL, ...) {
                               ## THIS NEEDS WORK
                               cb_args <- "e"
                               param_defn <- "var param={lat: e.latLng.lat(), lng: e.latLng.lng(), x: e.point.x, y: e.point.y};"
                               add_handler("mousemove", handler, action, cb_args=cb_args, param_defn=param_defn)
                             },
                             ##
                             ## some of the apie
                             set_center = function(center) {
                               "Center map on center, a lat long pair"
                               center <- sprintf("new google.maps.latLng(%s, %s)",
                                                 center[1], center[2])
                               call_method("setCenter", center)
                             },
                             set_zoom = function(zoom) {
                               "Set zoom level for map"
                               call_method("setZoom", as.integer(zoom))
                             },
                             set_size = function(width, height, animate=TRUE) {
                               "Set size of map"
                               call_method("setSize", width, height, animate)
                             },
                             set_maptype = function(maptype=c("roadmap", "satellite", "hybrid", "terrain")) {
                               maptype <- match.arg(maptype)
                               call_method("setMapTypeId", String(sprintf("google.maps.MapTypeId.%s",
                                                                    toupper(maptype))))
                             },
                             add_bikelayer = function(value=TRUE) {
                               "Add (or remove) a bicycle layer"
                               map <- ifelse(value, get_map(), "null")
                               
                               cmd <- paste("var bikeLayer = new google.maps.BicyclingLayer();",
                                            sprintf("bikeLayer.setMap(%s);", map),
                                            sep="")
                               add_js_queue(cmd)
                             },
                             add_trafficlayer = function(value=TRUE) {
                               "Add (or remove) a traffic layer"
                               map <- ifelse(value, get_map(), "null")
                               cmd <- paste("var trafficLayer = new google.maps.TrafficLayer();",
                                            sprintf("trafficLayer.setMap(%s);", map),
                                            sep="")
                               add_js_queue(cmd)
                             },
                             ## Below here is untested ... Could remove, as we made constructors
                             .df_to_array = function(df) {
                               "convert data frame to array"
                               res <- sapply(1:nrow(df), function(i) toJSObject(df[i,]))
                               out <- sprintf("[%s]", paste(res, collapse=","))
                               out
                             },
                             add_markers = function(markers) {
                               "Markers coerced to a JavaScript array. It can be a) a matrix or data frame with latlong pairs, with lat being first column, b) a character vector or data frame (first column) of strings to call geoCodeAddr. The optional column 'marker' holds text for the marker to display. Additional columns for setCenter and listeneres are possible, but not yet implemented"

                               if(is.null(dim(markers)))
                                 markers <- data.frame(markers, stringsAsFactors=FALSE)

                               if(is.numeric(markers[,1])) {
                                 ## a et of lat lng pairs
                                 nms <- c("lat", "lng", "marker", "setCenter", "listeners")
                                 names(markers) <- nms[1:ncol(markers)]
                               } else {
                                 ## geoCodeAddr bits
                                 nms <- c("geoCodeAddr", "marker", "setCenter", "listeners")
                                 names(markers) <- nms[1:ncol(markers)]
                               }
                               out <- .df_to_array(markers)
                               call_method("addMarkers", String(out))
                             },
                             add_polyline = function(points, linestyle) {
                               ## linestyle a list with strokeColor, strokeOpacity, strokeWeight
                               ## points a data frame or matrix of lat, lng points
                               if(is.matrix(points))
                                 points <- as.data.frame(points)
                               names(points) <- c("lat", "lng")
                               points_txt <- .df_to_array(points)

                               call_method("addPolyline", String(points_txt), linestyle)
                             },
                             create_info_window = function(inwin, point, marker) {
                               "Create Info Window, inwin: infowindow content, point: lat/lng pair, marker: title of marker "
                               inwin <- list(content=inwin)
                               point <- sprintf("new google.maps.LatLng(%s,%s)", point[1], point[2])
                               ## XXX add marker
                               marker <- sprintf("new google.maps.Marker({map:%s.getMap(), title:%s})", get_id(), marker)
                               call_method("createInfoWindow", inwin, String(point))
                             }
                             ))
                                 

##' base class for ggooglemaps objects
GGoogleMapsObject <- setRefClass("GGoogleMapsObject",
                                 contains="GWidget",
                                 method=list(
                                   write_constructor = function() {
                                     ## We need to pause here, as we wait for the map to be defined
                                     ## might need to pass 500 in through the map object
                                     cmd <- paste(sprintf("var %s; (new Ext.util.DelayedTask(function() {",
                                                          get_id()),
                                                  sprintf("%s = new %s(%s)",
                                                          get_id(),
                                                          constructor,
                                                          args$to_js_object()
                                                          ),
                                                  "})).delay(500);",
                                                  sep="")
                                     add_js_queue(cmd)
                                   },
                                   ## Could consolidate this with
                                   ## GGoogleMaps class above, but
                                   ## would still need to subclass
                                   ## write_constructor, so haven't
                                   add_R_callback = function(signal, cb_args="", param_defn="var param=null") {
                                     "Add handler, call on object, not map, not Ext guy"
                                     tpl <- "
var _fDelayed = function() {
  google.maps.event.addListener({{id}}, '{{signal}}', function({{args}}) {
    {{param_defn}};
    callRhandler('{{id}}', '{{signal}}', param)
  })
};
"
                                     cmd <- whisker.render(tpl, list(
                                                                     id=get_id(),
                                                                     signal=signal,
                                                                     args=paste(cb_args,collapse=","),
                                                                     param_defn=param_defn))
                                     add_js_queue(cmd)
                                     ## now call this function after a delay
                                     cmd <- paste("new Ext.util.DelayedTask(_fDelayed).delay(500);",
                                                  sep="")
                                     add_js_queue(cmd)
                                   }
                                    )) 

                                     

##' Add a marker to the map
##'
##' @param position lat/lng pair for position of mark
##' @param title optional tooltip text
##' @param icon option icon, stock name or url
##' @param map ggooglemaps instance
##' @return a GGoogleMapsObject
##' @export
##' @rdname ggooglemaps
ggooglemaps_marker <- function(position, title=NULL, icon=NULL, map) {
  gmm <- GGoogleMapsMarker$new(map$toplevel)
  gmm$init(position, title, icon, map)
  gmm
}


##' Class for markers
##'
##' The \code{GGoogleMapsMarker} class provides a class for marker objects on a map.
##' The main argument is \code{add_handler_clicked}, which allows one to assign a callback when a marker is clicked.
##' @rdname ggooglemaps
GGoogleMapsMarker <- setRefClass("GGoogleMapsMarker",
                                 contains="GGoogleMapsObject",
                                 fields=list(
                                   map="ANY", # GGoogleMap
                                   position = "ANY"
                                   ),
                                 methods=list(
                                   init=function(position, title, icon, map) {
                                     map <<- map
                                     position <<- position
                                     ## is icon a stock icon, url or NULL?
                                     if(!is.null(icon))
                                       icon <- getWithDefault(getStockIconByName(icon, css=FALSE), icon)
                                     
                                     constructor <<- "google.maps.Marker"
                                     arg_list = list(
                                       position=String(sprintf("new google.maps.LatLng(%s,%s)",
                                         position[1], position[2])),
                                       map = String(map$get_map()),
                                       title = title,
                                       icon = icon,
                                       draggable = FALSE
                                       )
                                     add_args(arg_list)
                                     write_constructor()
                                   },
                                   add_handler_clicked = function(handler, action=NULL, ...) {
                                     ## THIS NEEDS WORK
                                     cb_args <- "e"
                                     param_defn <- "tmp=e;var param={lat: e.latLng.lat(), lng: e.latLng.lng()};"
                                     add_handler("click", handler, action, cb_args=cb_args, param_defn=param_defn)
                                   }
                                   ))

##' Add an infowindow -- a popup cartoon -- to the map
##'
##' An info window allows one to display HTML code in a popup window
##' @param position Either a \code{ggooglemaps_marker} object or position as lat/lng pair
##' @param content HTML fragment to display in window
##' @param map a \code{ggooglemaps} object
##' @return a GGoogleMapsInfoWindow object
##' @export
ggooglemaps_infowindow <- function(position, content, map) {
  gmiw <- GGoogleMapsInfoWindow$new(map$toplevel)
  gmiw$init(position, content, map)
  gmiw
}

GGoogleMapsInfoWindow <- setRefClass("GGoogleMapsInfoWindow",
                                     contains="GGoogleMapsObject",
                                     fields=list(
                                       map="ANY",
                                       position="ANY"
                                       ),
                                     methods=list(
                                       init=function(position, content, map) {
                                         map <<- map
                                         if(is(position, "GGoogleMapsObject"))
                                           position <<- position$position
                                         else
                                           position <<- position

                                         constructor <<- "google.maps.InfoWindow"
                                         arg_list = list(
                                           position=String(sprintf("new google.maps.LatLng(%s,%s)",
                                             .self$position[1], .self$position[2])),
                                           content = content,
                                           map = String(map$get_map())
                                           )
                                         add_args(arg_list)
                                         write_constructor()
                                       }
                                       ))


##' A method to write a polyline to a map
##'
##' A polyline is equivalent to the \code{lines} function. The line is
##' specified as a data frame or matrix of latituted and longitude
##' pairs. The options include stroke values.
##' @param path a matrix or data frame of lat/lng pairs
##' @param stroke A list with values for color (hex RGB), opacity a
##' number in 0.0 to 1.0 and weight, a value for the weight of the
##' stroke.
##' @param map A ggooglemaps instance.
##' @param ... passed to init but ignored for now
##' @return an instance of Ggooglemapspolyline
##' @export
ggooglemaps_polyline <- function(path, stroke=list(color="#0000FF", opacity=1, weight=2), map, ...) {
  gmp <- GGoogleMapsPolyline$new(map$toplevel)
  gmp$init(path, stroke, map, ...)
  gmp
}
  


GGoogleMapsPolyline <- setRefClass("GGoogleMapsPolyline",
                                   contains="GGoogleMapsObject",
                                   fields=list(
                                     map="ANY", # GGoogleMap
                                     path="ANY"
                                     ),
                                   methods=list(
                                     init=function(path, stroke, map, ...) {
                                       
                                       
                                       constructor <<- "google.maps.Polyline"
                                       path <<- path
                                       
                                       arg_list = list(
                                         map = String(map$get_map()),
                                         path=String("a"),
                                         strokeColor = stroke$color,
                                         strokeOpacity = stroke$opacity,
                                         strokeWeight = stroke$weight
                                         )
                                       add_args(arg_list)
                                       write_constructor()
                                     },
                                     write_constructor = function() {
                                       add_js_queue(make_path(path))
                                       callSuper()
                                     },
                                     make_path = function(path) {
                                       "make a path from a data frame"
                                       paste("var a = new Array();",
                                             sprintf("vals = %s;", toJSArray(path)),
                                             "Ext.each(vals, function(i) {a.push(new google.maps.LatLng(i[0], i[1]))});",
                                             sep="")
                                     }

                                     ))
                                     

##' Draw a polygon on the map
##'
##' Draw a polygon specified as  matrix of data frame of latitude/longitude values
##' @param path path of polygon as matrix or data frame of points
##' @param stroke list of stroke parameters (color, opacity, weight)
##' @param fill list of fill parameters (color, opacity)
##' @param map ggooglemaps object
##' @param ... ignored
##' @return a polygon instance, but has no methods
##' @export
ggooglemaps_polygon <- function(path,
                                stroke=list(color="#0000FF", opacity=1, weight=2),
                                fill=list(color="#FF0000", opacity=0.25),
                                map, ...) {
  gmp <- GGoogleMapsPolygon$new(map$toplevel)
  gmp$init(path, stroke, fill, map, ...)
  gmp
}
  


##' base class for ggooglemaps polygon
GGoogleMapsPolygon <- setRefClass("GGoogleMapsPolygon",
                                   contains="GGoogleMapsPolyline",
                                  methods=list(
                                    init=function(path, stroke, fill, map, ...) {

                                      constructor <<- "google.maps.Polygon"
                                      path <<- path
                                      
                                      arg_list = list(
                                        map = String(map$get_map()),
                                        path=String("a"),
                                        strokeColor = stroke$color,
                                        strokeOpacity = stroke$opacity,
                                        strokeWeight = stroke$weight,
                                        fillColor = fill$color,
                                        fillOpacity = fill$opacity
                                        )
                                      add_args(arg_list)
                                      write_constructor()
                                    }
                                    ))
