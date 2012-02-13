##' run our "JavaScript RPC" call. (returns javascript, unlike JSON_RPC)
run <- function(...) {
  body <- rawToChar(request$body)
  l <- as.list(fromJSON(body))

  e <- session_manager$get_session_by_id(l$session_id)
  on.exit(session_manager$store_session(l$session_id, e))
  
  toplevel <- e[[".gWidgets_toplevel"]]
  toplevel$call_rpc(l$id, l$meth, l$value)
  txt <- toplevel$js_queue$flush()

  WebResult("html", txt, content.type="text/javascript")
}
