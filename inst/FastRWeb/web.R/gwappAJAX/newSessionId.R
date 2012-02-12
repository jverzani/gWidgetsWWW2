##' return a session id

run <- function(...) {

  id <- session_manager$get_id()
  
  out(toJSON(list(id=id)))
  done()
}
