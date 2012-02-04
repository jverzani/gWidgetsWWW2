#!/usr/bin/env Rscript

# Execute this file from the shell like so:
# Rook.sh 9000 &
# Rook.sh 9001 &
# Rook.sh 9002 &

## this is from Jeff Horner's Gist example.

## Needs some work -- start/stop/status type...

## what directory to scan for files or directories
app_dir <- "~/pmg/GW-refactor/gWidgetsWWW2/inst/examples/"

## do you want multiple instances?
multiple_instances <- FALSE

##################################################


require(gWidgetsWWW2)
session_manager <- if(multiple_instances) {
  gWidgetsWWW2:::make_session_manager(use_filehash=TRUE)
} else {
  gWidgetsWWW2:::make_session_manager()
}


## Read from command line
port <- as.numeric(commandArgs(trailingOnly=TRUE))
Rk <- gWidgetsWWW2:::.load_gwidgets_base(port, session_manager)

files <- Sys.glob(sprintf("%s*", app_dir))
QT <- sapply(files, function(f) {
  if(file.info(f)$isdir)
    gWidgetsWWW2:::load_dir(f, session_manager=session_manager, R_httpd=Rk$R, open_page=FALSE, show.log=TRUE)
  else
    gWidgetsWWW2:::load_app(f, session_manager=session_manager,  R_httpd=Rk$R, open_page=FALSE, show.log=TRUE)
})


while(TRUE) Sys.sleep(.Machine$integer.max)
