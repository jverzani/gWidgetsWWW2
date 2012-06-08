#!/usr/bin/env Rscript

# Execute this file from the shell like so:
# Rook.sh 9000 &


## what directory to scan
## CHange to something appropriate
app_dir <- "/var/FastRWeb/gw_apps"

## do you want multiple instances?
## Multiple instances are much slower due to the need to lookup the evaluation
## environment, de-serialize it, evaluate, then serialize.
multiple_instances <- FALSE

##################################################


require(gWidgetsWWW2)
session_manager <- gWidgetsWWW2:::make_session_manager()



## Read port from command line
port <- as.numeric(commandArgs(trailingOnly=TRUE))

## Rk is our Rook instance
Rk <- gWidgetsWWW2:::R_http$get_instance()
Rk$start(port) ## Rk$start(port, listen="external.ip") ## for an external ip
Rk$load_gw()


files <- Sys.glob(sprintf("%s*", app_dir))
QT <- sapply(files, function(f) {
  if(file.info(f)$isdir)
    Rk$load_dir(f, session_manager=session_manager, open_page=FALSE, show.log=TRUE)
  else
    Rk$load_app(f, session_manager=session_manager, open_page=FALSE, show.log=TRUE)
})

## prevent it from closing
while(TRUE) Sys.sleep(.Machine$integer.max)
