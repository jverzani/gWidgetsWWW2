## Append this to the end of the rserve.R file
options("gWidgetsWWW2:FastRWeb"=TRUE)

require(Rserve)
require(gWidgetsWWW2)
##require(filehash)

## session management is a big issue. We use a file to store the evaluation environment.
## this is a *perfect* task for the filehash package, but we implement our own, as
## there are speed improvements

## How to use filehash
## session_manager <- gWidgetsWWW2:::make_session_manager(use_filehash=TRUE)

##' uncomment to send out log messages
logger <- function(...) {
  ##message(...)
}

session_manager <- setRefClass("SessionManager",
                               fields=list(
                                 session_dir="character"
                                 ),
                               methods=list(
                                 initialize=function(d="/tmp/sessions", ...) {
                                   if(length(list.dir(d)) == 0)
                                     dir.create(d)
                                   initFields(session_dir = d)
                                   callSuper(...)
                                 },
                                 make_file = function(id) {
                                   sprintf("%s%s%s", session_dir, .Platform$file.sep, id)
                                 },
                                 lock_file_name = function(id) {
                                   sprintf("%s_lock", make_file(id))
                                 },
                                 is_locked = function(id) {
                                   file.exists(lock_file_name(id))
                                 },
                                 lock_file = function(id) {
                                   cat("lock", file=lock_file_name(id))
                                 },
                                 unlock_file = function(id) {
                                   unlink(lock_file_name(id))
                                 },
                                 id_exists=function(id) {
                                   file.exists(make_file(id))
                                 },
                                 get_id=function() {
                                   id <- paste(sample(c(LETTERS,0:9), 10, replace=TRUE), collapse="")
                                   id ## check for repeats
                                 },
                                 get_session_by_id=function(id) {
                                   if(!id_exists(id))
                                     return(NULL)

                                   if(!is_locked(id)) {
                                     lock_file(id)
                                     out <- try(readRDS(make_file(id)), silent=TRUE)
                                     return(out)
                                   }
                                   ## else we work
                                   ctr <- 0; MAX_CT <- 10000 # how high should this be?
                                   while(is_locked(id) && ctr < MAX_CT) {
                                     Sys.sleep(0.1)
                                     ctr <- ctr + 1
                                   }
                                   if(ctr >= MAX_CT) {
                                     logger("*** Failed to get lock after ***", ctr)
                                     NULL
                                   } else {
                                     lock_file(id)
                                     readRDS(make_file(id))
                                   }
                                 },
                                 store_session=function(id, e) {
                                   on.exit(unlock_file(id))
                                   saveRDS(e, make_file(id))
                                 },
                                 clear_session=function(id) {
                                   unlink(make_file(id))
                                   unlock_file(id)
                                 }
                                 ))$new()



