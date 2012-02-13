##' Script to pass tmp files back from the browser
##' Slight change From Simon Urbanek's original with FastRWeb

content_types <- function(x) {
  Rook:::Mime$mime_type(Rook:::Mime$file_extname(x))
}

run <- function(file, mime="text/html",...) {
  mime <- content_types(file)
  WebResult("tmpfile", gsub("/", ".", file, fixed=TRUE), mime)
}
