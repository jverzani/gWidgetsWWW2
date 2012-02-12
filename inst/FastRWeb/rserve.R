## Append this to the end of the rserve.R file
require(Rserve)
require(gWidgetsWWW2)
require(filehash)

## for now, we use the filehash option. Going forward we aim to avoid this.
message("Making sesssion_manager global instance")
session_manager <- gWidgetsWWW2:::make_session_manager(use_filehash=TRUE)

