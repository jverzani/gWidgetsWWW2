##' Find file in the post body. Heavy lifting done by parse.multipart from J. Horner

run <- function(...) {


  mp <- parse.multipart()
  
  if(!is.null(mp)) {
    session_id <- mp$session_id; mp$session_id <- NULL
    id <- mp$id; mp$id <- NULL
    if(length(mp)) {
      mp <- mp[[1]]                       # has funny name
      e <- session_manager$get_session_by_id(session_id)
      on.exit(session_manager$store_session(session_id, e))
      toplevel <- e[[".gWidgets_toplevel"]]
      obj <- toplevel$get_object_by_id(id)
      
      fname <- mp$tempfile
      orig_name <- mp$filename
      
      if(file.exists(fname)) {
        obj$set_value(fname)
        obj$set_filename(orig_name)
        out("{success:true}")
      } else {
        out("{success:false}")          # no expecting this
      }
    } else {
      out("{success:false}")   # file removed can cause this
    }
  } else {
    out("{success:false}")    
  }
  done()
  
}
