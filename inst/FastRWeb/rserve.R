## Append this to the end of the rserve.R file
options("gWidgetsWWW2:FastRWeb"=TRUE)

require(Rserve)
require(gWidgetsWWW2)
## we use a file to store the sessions. This is slower, but the only way we
## have this working with FastRWeb right now.
## @param d is a directory. It needs to be writeable by the RServe process
session_manager <- gWidgetsWWW2:::make_session_manager(use.filehash=TRUE, d="/tmp/sessions")
