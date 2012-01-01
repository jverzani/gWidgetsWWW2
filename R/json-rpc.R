##' A JSON-RPC class


##' we expect a callback into the R session that looks like:
##' @param obj name of aan JSON-rpc object in the global namespace
##' @param method name an exported method of obj
##' @param JSON encoded parameters to pass to the method call
##' jsonRpc([{obj, method, params},{,,}])
##' we return JSON (or script)
##'



JsonRPCServer <- setRefClass("JsonRPCServer",
                       contains="Middleware",
                       fields=list(
                         exported="character",
                         return_type="character",
                         loaded="logical"
                         ),
                       methods=list(
                         initialize=function(return_json=TRUE,
                           ...) {

                           initFields(
                                      return_type=ifelse(return_json, "json", "javascript"),
                                      loaded=FALSE
                                      )

                           callSuper(...)

                         },
                         call = function(env) {
                           req <- Request$new(env)

                           if(!req$post()) {
                             stop(gettext("Call with a POST request"))
                           }

                           l <- read_rook_input(req)
                           print(l)

                           ## loop over items in l,
                           ## out += get(item$obj)$call_method(item$method, item$params)

                           out <- lapply(l, function(item) {
                             get(item$obj)$call_method(item$meth, item$params)
                           })

                           res <- Response$new(status=200L,
                                               headers=list('Content-Type'=sprintf("application/%s", return_type)),
                                               body=paste(out, collapse="\n")
                                               )
                           res$write("")
                           res$finish()
                         },
                         read_rook_input = function(req) {
                           "Read rook.input, then convert from JSON"
                           req$env[['rook.input']]$rewind()
                           input <- req$env[['rook.input']]$read()
                           l <- fromJSON(rawToChar(input))
                           if(!is.list(l))
                             l <- sapply(l, identity, simplify=FALSE)
                           l
                         },
                         load=function(url="JSON_RPC", port=9000) {
                           if(loaded)
                             return()
                           
                           R <- Rhttpd$new()
                           try(R$start(port=port), silent=TRUE)
                           R$add(RhttpdApp$new(.self, name=url))
                         }
                         ))


.json_rpc_server <- function(url="JSON_RPC", port=9000) {
  JsonRPCServer$new()$load(url=url, port=port)
}

json_rpc_server <- memoise(.json_rpc_server)


JsonRPCObject <- setRefClass("JsonRPCObject",
                             fields=list(
                               exported="character"
                               ),
                             methods=list(
                               initialize=function(methods=character(0),
                                 ...) {
                                 
                                 exported <<- methods

                                 callSuper(...)
                               },
                               export_method=function(meth) {
                                 exported <<- c(exported, meth)
                               },
                               call_method=function(meth, lst) {

                                 if(meth %in% exported) {
                                   if(exists(meth, .self, inherits=FALSE))
                                     f <- get(meth, .self)
                                   else
                                     f <- methods:::envRefInferField(.self, meth, getClass(class(.self)), .self)

                                   out <- do.call(f, lst)
                                   ## should convert out to JSON!
                                 } else {
                                   out <- ""
                                 }
                                 out
                               }
                               ))

                               
                       
## test it out
JsonRPCObjectA <- setRefClass("JsonRPCObjectA",
                              contains="JsonRPCObject",
                              methods=list(
                                initialize=function(...) {

                                  methods=c("meth")

                                  callSuper(methods, ...)
                                },
                                meth=function(a,b,c) {
                                  x <- iris
                                  names(x) <- c("sepalLength","sepalWidth","petalLength","petalWidth","Species")
                                  list(traits=names(x),
                                       species=levels(x$Species),
                                       values=x)
                                }
                                ))

a <- JsonRPCObjectA$new()

## call with Ext, say, like this:
## Ext.Ajax.request({
##     url:'/custom/JSON_RPC',
##     jsonData:[{
## 	obj:'a',
## 	meth:'meth',
## 	params:{a:1,b:'two',c:false}
##     }
## 	     ], 
##     success:function(response) {
##               var output = response.responseText;
##             }
## });
