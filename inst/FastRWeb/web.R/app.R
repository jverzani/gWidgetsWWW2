##' interface to run an app from the directory /var/FastRWeb/gw_apps


run <- function(app, ...) {
  app_dir <- getOption("gWidgetsWWW2:app_dir")
  if(is.null(app_dir))
    app_dir <- "/var/FastRWeb/gw_apps"

  ## is app file or directory?
  if(file.exists(x <- paste(app_dir, .Platform$file.sep,  app, ".R", sep=""))) {
    ## we will load x
  } else {
    print(x)
    stop("Can't find app")
  }

  txt <-  "
<html>
<head>
  <script  type='text/javascript' src='http://localhost/cgi-bin/R/gWidgetsWWW2?name=javascript/jquery-1.7.1.min.js'></script>
  <script  type='text/javascript' src='http://localhost/cgi-bin/R/gWidgetsWWW2?name=javascript/ext/ext-all.js'></script>

  <script type='text/javascript' src='/cgi-bin/R/gWidgetsWWW2?name=javascript/gw-gtable.js'></script>
  <script type='text/javascript' src='/cgi-bin/R/gWidgetsWWW2?name=javascript/ext.ux.canvas.js'></script>
  <script type='text/javascript' src='/cgi-bin/R/gWidgetsWWW2?name=javascript/ext.ux.example.js'></script>
  <script type='text/javascript' src='/cgi-bin/R/gWidgetsWWW2?name=javascript/ext.ux.CheckColumn.js'></script>
  <script type='text/javascript' src='/cgi-bin/R/gWidgetsWWW2?name=javascript/ext.ux.CustomSpinner.js'></script>


  <script type='text/javascript'  src='/cgi-bin/R/gWidgetsWWW2?name=javascript/GMapPanel3.js'></script>

  <script type='text/javascript' src='/cgi-bin/R/gWidgetsWWW2?name=javascript/ext.ux.CodeMirror.js'></script>
  <script type='text/javascript'  src='/cgi-bin/R/gWidgetsWWW2?name=javascript/CodeMirror/codemirror.js'></script>
  <script type='text/javascript' src='/cgi-bin/R/gWidgetsWWW2?name=javascript/CodeMirror/formatting.js'></script>
  <script type='text/javascript'  src='/cgi-bin/R/gWidgetsWWW2?name=javascript/CodeMirror/foldcode.js'></script>
  <script type='text/javascript' src='/cgi-bin/R/gWidgetsWWW2?name=javascript/CodeMirror/r.js'></script>
  <script type='text/javascript' src='/cgi-bin/R/gWidgetsWWW2?name=javascript/CodeMirror/emacs.js'></script>


  <link rel='stylesheet' type='text/css' href='http://localhost/cgi-bin/R/gWidgetsWWW2?name=javascript/ext/resources/css/ext-all-gray.css'/>
  <link rel='stylesheet' type='text/css' href='http://localhost/cgi-bin/R/gWidgetsWWW2?name=css/gWidgetsWWW2.css' />
  <script type='text/javascript' src='http://localhost/cgi-bin/R/load_AJAX'></script>

</head>
<body>
<script type='text/javascript'>
$(document).ready(function() {
  $.ajax('{{base_url}}' + 'newSessionId', {
    dataType: 'json',
    cache: false,
    success: function(data) {
      session_id = data.id;
      createGUI('{{ app_url }}', session_id, '{{app}}');
    }
  })
});

</script>
</body>
</html>
"

  txt <- whisker.render(txt, list(base_url="/cgi-bin/R/gwappAJAX/", app_url="/cgi-bin/R/gwappAJAX/createGUI", app=app))
  out(txt)
  done()
  
}
