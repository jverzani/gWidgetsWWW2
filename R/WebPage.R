## Template for a webpage to display a gWidgetsWWW2 app
## user can override \code{doc_type}, \code{content_type},
## \code{head} and \code{body}. These can be strings or filenames that will
## be rendered as seen fit

WebPage <- setRefClass("WebPage",
                       contains="Middleware",
                       fields=list(
                         "url"="character",
                         "app_name"="character",
                         "renderer"="character",
                         "data"="list", # for evaluation
                         .body="character",
                         .head="character"
                         ),
                       methods=list(
                         initialize=function(
                           url="/",
                           app_name="test",
                           renderer=c("whisker", "brew", "none"),
                           data=list(),
                           body=" ",
                           head="<title></title>",
                           ...) {
                           
                           initFields(url=url,
                                      app_name=app_name,
                                      renderer=match.arg(renderer),
                                      data=data
                                    )

                           if(is.function(body)) 
                             .body <<- body()
                           else
                             .body <<- body

                           if(is.function(head))
                             .head <<- head()
                           else
                             .head <<- head
                           
                           
                           callSuper(...)
                         },
                         doc_type=function() {
                           "<!DOCTYPE HTML>"
                         },
                         content_type = function() {
                           "<meta http-equiv='Content-Type' content='text/html; charset=UTF-8'/>"
                         },
                         head=function() {
                           "Add additional head content"
                           .head
                         },
                         body=function() {
                           "Insert additional body code here"
                           .body
                         },
                         ### 
                         render=function() {
                           paste(do_chunk(doc_type()),
                                 "<html><head>",
                                 do_chunk(content_type()),
                                 do_chunk(head()),
                                 load_css(),
                                 make_icons(),
                                 load_jquery(),
                                 load_AJAX(),
                                 on_ready(),
                                 "</head>",
                                 "<body>",
                                 loading_msg(),
                                 load_gw_js(),
                                 do_chunk(body()),
                                 "</body>",
                                 "</html>",
                                 sep="\n")
                         },
                         call=function(env) {
                           out <- render()
                           res <- Response$new(status=200L,
                                               headers=list('Content-Type'='text/html'),
                                               body=out
                                               )
                           
                           res$write("")
                           res$finish()
                         },
                         ## These all return a filename or a character vector
                         ## we check by a) of class FileName or b) file.exists on [1]
                       do_chunk=function(val) {
                         if(renderer == "whisker")
                           do_whisker(val)
                         else
                           do_brew(val)
                       },
                       do_whisker=function(val) {
                         if(is(val, "FileName") || file.exists(val[1])) {
                           out <- sapply(val, function(..i) {
                             whisker.render(paste(readLines(..i), collapse="\n"), data=data)
                           })
                         } else {
                           out <- sapply(val, whisker.render, data=data)
                         }
                         paste(out, collapse="\n")
                       },
                         do_brew=function(val) {
                           out_file <- tempfile()
                           con <- file(out_file)
                           ## How to add to data to evaluation environment?
                           if(is(val, "FileName") || file.exists(val[1])) {
                             sapply(val, brew, output=con)
                           } else {
                             f <- function(x, file, ...) brew(text=x, ...)
                             out <- sapply(val, f, output=con)
                           }
                           close(con)
                           out <- paste(readLines(out_file), collapse="\n")
                           unlink(out_file)
                           out
                         },
                         do_none=function(val) {
                           if(is(val, "FileName") || file.exists(val[1])) {
                             out <- paste(sapply(val, function() paste(readLines(i), collapse="\n"), collapse="\n"))
                           } else {
                             paste(val, collapse="\n")
                           }
                         },
                         loading_msg=function() {
                                   "
<div id='loading'>
   <div class='loading-indicator'>
    <span id='loading-msg'>Loading ExtJS...</span>
    </div>
  </div>
  <div id='load_app'></div>
"
                                 },
                         
                       load_css=function() {
                         "
<link rel='stylesheet' type='text/css' href='/custom/gWidgetsWWW2/javascript/ext-4-2.1/resources/css/ext-all.css' />
<link rel='stylesheet' type='text/css' href='/custom/gWidgetsWWW2/javascript/CodeMirror/codemirror.css'  />
<link rel='stylesheet' type='text/css' href='/custom/gWidgetsWWW2/css/gWidgetsWWW2.css'    />
"
                       },
                       make_icons=function() {
                         paste("<style type='text/css'>\n",
                               paste(make_stock_icons(), collapse="\n"),
                               "</style>",
                               sep="\n")
                      },
                       load_jquery=function() {
                         "
<script type='text/javascript'
  src='/custom/gWidgetsWWW2/javascript/jquery-1.7.1.min.js'>
</script>
"
                       },
                       load_AJAX=function() {

                         x <- system.file('framework', 'templates', 'default_ajax.js', package='gWidgetsWWW2')
                         sprintf("<script type='text/javascript'>%s</script>",
                                 whisker.render(paste(readLines(x), collapse="\n"),
                                                list(base_url="/custom")))
                                                     

                       },
                       on_ready=function() {
                         tpl <- "
<script type='text/javascript'>
$(document).ready(function() {
  Ext.QuickTips.init();
  $('#loading').hide();
  $.ajax(base_url + 'newSessionId', {
    dataType: 'json',
    cache: false,
    success: function(data) {
      session_id = data.id;
      $('#load_app').innerHTML='Loading app... ';
      createGUI('{{app_url}}')
    }
  })
});
  </script>
"
                        whisker.render(tpl, list(app_url=sprintf("/custom/app_%s/",
                                                   app_name)))
                       },
                       load_gw_js=function() {
                         x <- system.file("framework", "templates", "load_js.html", package="gWidgetsWWW2")
                         whisker.render(paste(readLines(x), collapse="\n"),
                                                list(base_url="/custom"))
                       }
))
