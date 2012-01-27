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

To use `gWidgetsWWW2`, one writes scripts to be executed by Rook.
The basic "Hello World" app with a window, container, button and
callback is created in a script `hello.R` and contains:


    
    w <- gwindow("Hello world example")
    g <- ggroup(cont=w, width=300, height=300)
    b <- gbutton("click me", cont=g, handler=function(h,...) {
      galert("Hello World", parent=w)
    })
    
This script is turned into a web page through the +load_app+ function:
    
    require(gWidgetsWWW2)
    load_app("hello.R", app_name="hello")
    

The app is then mapped to the url

     http://127.0.0.1:PORT/custom/hello/indexgw.rhtml


The "PORT" will be that for R's internal help server. The `port`
argument can pass in a value if that has not yet been started, with a
default of 9000.

The "hello" in the url matches that given by the `app_name` argument.


Installation
------------

Installing the package from GitHub is made easy if the `devtools`
package of H. Wickham is installed:

    library(devtools)
    install_github("gWidgetsWWWW2", "jverzani")
    

Otherwise, a) install git, b) clone the project c) use +R CMD
INSTALL+, or some such to install from a local set of files.

Serving pages through a web server, nginx
-----------------------------------------

The package uses the `Rook` package of J. Horner to serve web pages
through `R`'s internal web server (for serving help pages). This can be
exposed to the wider world, or a proxy can be used. To configure the
`nginx` server to proxy, require two steps. These are for an Ubuntu istallation:

In /etc/nginx/sites-enabled, add this to the "default" file in the `server` configuration


	location /custom {
	  proxy_pass http://localhost:9000/custom;
        }


In /etc/nginx.conf, add this to in the `http` configuration


	upstream rookapp {
        	 server localhost:9000;
	}

Then start R with port 9000 and load the apps you want.


The `custom` path name is from the internal help server and could be
changed in the location directive. The above uses port 9000, the
default. For more information on using nginx in front of Rook, see
this post: https://gist.github.com/6d09536d871c1a648a84

The example in the gist starts 3 R processes. If you are going to do
that, you should use the filehash options for the session manager, as
this allows the session environements to be shared.


RApache
-------

As of RApache 1.1.15 there is now support for Rook apps. At some point
this should be integrated into that framework, but initial attempts
were thwarted by two issues: handling of POST requests and more
importantly speed issues. 

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

