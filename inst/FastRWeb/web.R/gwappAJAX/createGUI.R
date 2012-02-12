##' create the GUI


run <- function(session_id, app, ...) {
  
  app_dir <- getOption("gWidgetsWWW2:app_dir")
  if(is.null(app_dir))
    app_dir <- "/var/FastRWeb/gw_apps"
  the_script <- paste(app_dir, .Platform$file.sep,  app, ".R", sep="")
  
  e <- session_manager$get_session_by_id(session_id)
  on.exit(store_session(session_id, e))
  
  if(is.null(e)) {
    e <- new.env()
    
    ## create a toplevel element and place within an evaluation environment
    toplevel <- gWidgetsWWW2:::GWidgetsTopLevel$new()
    toplevel$set_e(e)
    assign(".gWidgets_toplevel", toplevel, env=e)
    lockBinding(".gWidgets_toplevel", env=e)
    
    ## debug
    assign(".toplevel", toplevel, .GlobalEnv)
  } else {
    toplevel <- get_toplevel(e=e)
  }
  
  
  
  attach(e) # attach/detach allows one to find toplevel
  out <- try(sys.source(the_script, envir=e), silent=TRUE)
  detach(e)

  cmd <- sprintf("var session_id='%s';", session_id)
  toplevel$js_queue$push(cmd)
  ## returns javascript commands
  x <- toplevel$js_queue$flush()


  out(paste(x, collapse="\n"))
  
  done()
  
}
