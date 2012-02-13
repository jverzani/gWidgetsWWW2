##' transport is meant to send data from client to R process

run <- function(...) {
  body <- rawToChar(request$body)
  l <- as.list(fromJSON(body))

  e <- session_manager$get_session_by_id(l$session_id)
  on.exit(session_manager$store_session(l$session_id, e))
  
  toplevel <- e[[".gWidgets_toplevel"]]
  obj <- toplevel$get_object_by_id(l$id)

  
  param <- try(fromJSON(l$param, asText=TRUE), silent=TRUE)
  if(inherits(param, "try-error")) {
    print(list("erro", param))
    stop("error will rogers")
  }
  param <- as.list(param)

  do.call(obj$process_transport, param)
  
  txt <- toplevel$js_queue$flush()
  out(txt)
  done()
}
