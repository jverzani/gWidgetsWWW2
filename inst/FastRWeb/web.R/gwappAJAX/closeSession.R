##' close the session
##'


run <- function(session_id, ...) {

  session_manager$clear_session(session_id)

  done()
}
                
