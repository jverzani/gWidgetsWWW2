##' process a "JSON_RPC" request
##'
##' One needs to create an object with exported methods to use this.
##' For example, if this is defined in rserve.R (or sourced in)
##' 
##' JSON_TEST <- setRefClass("JSON_TEST",
##'                          contains="JsonRPCObject",
##'                          methods=list(
##'                            test_method=function(...) {
##'                              toJSON(list(a="hello", b="world"))
##'                            }
##'                            ))$new()
##' JSON_TEST$export_method("test_method")
##' then a call from the browser of:
##' json_rpc("JSON_TEST", "test_method", null)
##'
##' Will return the value { "a": "hello","b": "world" }
##' One would likely pass a callback -- a JavaScript function -- to process
##' this value on a successful return.

run <- function(...) {
  ## values come through by post
  body <- rawToChar(request$body)
  l <- as.list(fromJSON(body))

  obj <- get(l$obj, inherits=TRUE)
  meth <- l$meth
  params <- l$params

  if(is(obj, "JsonRPCObject")) {
    tmp <- obj$call_method(meth, params)
    if(length(tmp) > 1)
      tmp <- sprintf("[%s]", paste(tmp, collapse=","))
    out(tmp)
  } else {
    out("")
  }
  done()
}
  
