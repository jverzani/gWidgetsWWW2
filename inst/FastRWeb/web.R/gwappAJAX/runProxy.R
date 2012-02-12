##' Run a proxy

run <- function(session_id, id, `_dc`, ...) {

  e <- session_manager$get_session_by_id(session_id)
  on.exit(session_manager$store_session(session_id, e))
  toplevel <- e[[".gWidgets_toplevel"]]



  ## do things different if we have a POST
  if(!is.null(request$body)) {
    ## for some calls (gdf editing) parameters are passed back through request$body
    body <- rawToChar(request$body)
    params <- as.list(fromJSON(body))
    txt <- toplevel$call_post_proxy(id, params)
    content_type <- "text/json"
  } else {
    params <- list(...)
    txt <- toplevel$call_proxy(id, params)
    content_type <- "text/javascript"
  }
  
  WebResult("html", txt, content.type=content_type)  
}
