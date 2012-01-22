#require(gWidgetsWWW2)
## This handles the AJAX calls
ajax_app<-Builder$new(
        gWidgetsWWW2:::GWidgetsAppAjax$new(
 	        session_manager=gWidgetsWWW2:::make_session_manager(use_filehash=TRUE)
              ),
        Redirect$new('/no_mas.txt')
        )
