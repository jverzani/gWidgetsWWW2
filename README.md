Port of gWidgets to the web
---------------------------

This package implements the `gWidgets` API for web
programming. Primarily, the package turns the gWidgets commands into
`JavaScript` commands. The `ExtJS` javascript libraries (www.sencha.com)
are used for this, as they provide an excellent programming
environment for `JavaScript`. Of course, the `R` user need not know any
`JavaScript`, or even any web technologies -- not even HTML. Basically,
most simple gWidgets scripts will just work.

Example
-------

To use `gWidgetsWWW2` locally, one writes scripts to be executed by Rook.
The basic "Hello World" app with a window, container, button and
callback is created in a script `hello.R` and contains:


    
    w <- gwindow("Hello world example")
    g <- ggroup(cont=w, width=300, height=300)
    b <- gbutton("click me", cont=g, handler=function(h,...) {
      galert("Hello World", parent=w)
    })
    
This script is turned into a web page through the `load_app` function:
    
    require(gWidgetsWWW2)
    load_app("hello.R", app_name="hello")
    

The app is then mapped to the url

     http://127.0.0.1:PORT/custom/hello/indexgw.rhtml


The "PORT" will be that for R's internal help server. The `port`
argument can pass in a value if that has not yet been started, with a
default of 9000.

The "hello" in the url matches that given by the `app_name` argument. 

Read on for notes on deployment beyond the local computer.


Installation
------------

Installing the package from GitHub is made easy if the `devtools`
package of H. Wickham is installed:

    library(devtools)
    install_github("gWidgetsWWWW2", "jverzani")
    

Otherwise, a) install git, b) clone the project c) use +R CMD
INSTALL+, or some such to install from a local set of files.


Graphics
--------

There is no interactive graphic device, though one can use several
non-interactive graphics devices, in particular: `canvas`, through the
`gcanvas` widget; `png`, through the `gimage` widget; and `RSVGTipsDevice`,
through the `gsvg` widget. The basic use is all similar. Here is an
example for the canvas device, which writes `JavaScript` to an HTML5
canvas object in the web page:


    require(canvas) 
    w <- gwindow("Test")
    gstatusbar("Powered by gWidgetsWWW2 and Rook", cont=w)
    g <- ggroup(cont=w, horizontal=FALSE, height=500, width=500)
    make_plot <- function() {
      f <- tempfile()
      canvas(f, width=400, height=400)
      hist(rnorm(100))
      dev.off()
      f
    }
    #
    cnv <- gcanvas(make_plot(), width=400, height=400, cont=g)
    btn <- gbutton("new graph", cont=g, handler=function(h,...) {
      svalue(cnv) <- make_plot()
    })


Deploying pages to the internet
===============================

Deploying pages to the internet can be done in a few manners:

* with `Rook` one can listen to an external IP

* with `nginx` (a web server) one can proxy external requests to a local `Rook` application

* with `FastRWeb` one can use `R` through `Rserve`

* Currently, the package is not integrated in with `rapache`.

Of these, the `FastRWeb` solution is suggested. Details for the
necessary setup are in the `FastRWeb` sub directory of the package's
`inst/` directory.

