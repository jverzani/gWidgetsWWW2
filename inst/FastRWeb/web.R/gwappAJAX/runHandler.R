##' run the handlers
##'
run <- function(...) {
  body <- rawToChar(request$body)
  l <- as.list(fromJSON(body))
  
  e <- session_manager$get_session_by_id(l$session_id)
  on.exit(session_manager$store_session(l$session_id, e))
  toplevel <- e[[".gWidgets_toplevel"]]

  value <- try(fromJSON(l$value, asText=TRUE), silent=TRUE)
  toplevel$call_handler(l$id, l$signal, l$value, new.env())
  
  txt <- toplevel$js_queue$flush()
  out(txt)
  done()
}


runJW <- function(session_id, id, signal, value, ...) {


  e <- session_manager$get_session_by_id(session_id)
  on.exit(session_manager$store_session(session_id, e))

  print(list("runHandler", value))
  value <- fromJSON(value, asText=TRUE)

  
  toplevel <- e[[".gWidgets_toplevel"]]
  
  
  toplevel$call_handler(id, signal, value, new.env())



  
  txt <- toplevel$js_queue$flush()
  
  out(txt)
  done()
}
