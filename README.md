Port of gWidgets to the web
---------------------------

This package implements the `gWidgets` API for web
programming. Deployment is through `Rook`, best suited for local use
or limited internet usage. For wider deployment, there is
`gWidgetsWWW2.rapache` which runs under Jeffrey Horner's rapache
module for the apache webserver.

 Primarily, the package turns the gWidgets commands into
`JavaScript` commands. The `ExtJS` javascript libraries (www.sencha.com)
are used for this, as they provide an excellent programming
environment for `JavaScript`. Of course, the `R` user need not know any
`JavaScript`, or even any web technologies -- not even HTML. Basically,
most simple gWidgets scripts will just work.

If installed from github, the extjs files will be downloaded. If
installed as a R package, the commmand `download_extjs` must be issued
to download the extjs files (67Mb) and isntall them within the package
directory.


Example
-------

To use `gWidgetsWWW2` locally, one writes scripts to be executed by Rook.
The basic "Hello World" app with a window, container, button and
callback is created in a script `hello.R` and contains:


```    
    w <- gwindow("Hello world example")
    g <- ggroup(cont=w, width=300, height=300)
    b <- gbutton("click me", cont=g, handler=function(h,...) {
      galert("Hello World", parent=w)
    })
```
    
This script is turned into a web page through the `load_app` function:
    
```
    require(gWidgetsWWW2)
    load_app("hello.R", app_name="hello")
```    

The app is then mapped to the url

>     http://127.0.0.1:PORT/custom/hello/indexgw.rhtml


The "PORT" will be that for R's internal help server. The `port`
argument can pass in a value if that has not yet been started, with a
default of 9000.

The "hello" in the url matches that given by the `app_name` argument. 

Read on for notes on deployment beyond the local computer.


Installation
------------

You can install the package from RForge:
```
    install.packages("gWidgetsWWW2", repos = "http://r-forge.r-project.org")
```    

Alternatively you can install from github, if the `devtools` package of H. Wickham is installed:

```
    library(devtools)
    install_github("jverzani/gWidgetsWWW2")
```    

Otherwise, a) install git, b) clone the project c) use +R CMD
INSTALL+, or some such to install from a local set of files.


Graphics
--------

There is no interactive graphic device, though one can use several
non-interactive graphics devices, in particular: `png`, through the `gimage` widget; and `RSVGTipsDevice`,
through the `gsvg` widget. The basic use for each is similar.
