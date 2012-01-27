## This handles the AJAX calls
ajax_app<-Builder$new(
        gWidgetsWWW2:::GWidgetsAppAjax$new(
 	        session_manager=..session_manager..
              ),
        Redirect$new('/no_mas.txt')
        )
