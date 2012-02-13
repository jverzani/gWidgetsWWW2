##' run proxy for ghtml widget


run <- function(session_id, id, ...) {
  e <- session_manager$get_session_by_id(session_id)
  on.exit(session_manager$store_session(session_id, e))

  toplevel <- e[[".gWidgets_toplevel"]]

  obj <- toplevel$get_object_by_id(id)
  txt <- obj$get_json_data()
  
  out(txt)
  done()
}
