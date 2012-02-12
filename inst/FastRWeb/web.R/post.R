##' How to get post to work

## call from jquery with
## This works with GET
## data = {id: 'id', signal:'signal'};
## $.ajax({
##     url:'/cgi-bin/R/post',
##     data: data,
##     dataType:'script',		  
##     cache: false,
##     type:'GET'
## });

## POST?
## Jquery command -- works with GET, not with POST
## data = {id: 'id', signal:'signal'};
## $.ajax({
##     url:'/cgi-bin/R/post',
##     data: data,
##     dataType:'script',		  
##     cache: false,
##     type:'POST'
## });

run <- function(...) {
  l <- list(qs=qs, pars=pars, cmd=cmd, ct=ct,hdr= hdr)
  print(rawToChar(request$body)) ## to console
  out("alert('testing');")
  done()
}
