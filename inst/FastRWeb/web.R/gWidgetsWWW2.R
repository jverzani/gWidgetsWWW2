##' Serve up js/css/png files from the gWidgetsWWW2 package

content_types <- function(x) {
  Rook:::Mime$mime_type(Rook:::Mime$file_extname(x))
}

run <- function(name, ...) {
  x <- system.file("base", name, package="gWidgetsWWW2")

  is_text_file <- function(x) {
    grepl("txt$|css$|js$|R$", x)
  }

  if(is_text_file(x)) {
    x <- readLines(x)
    ## need to give different urls to '/resources/'
    x <- gsub("../../resources/", "/cgi-bin/R/gWidgetsWWW2?name=javascript/ext/resources/", x)
    WebResult("html", payload=paste(x, collapse="\n"), content.type=content_types(name), header="Cache-Control: max-age=3600, must-revalidate")
  } else {
    ## return file
    ## not x but name -- relative to web/
    ## I think only binary files from here are image/png -- generalize if need be
    WebResult("file", payload=name, content.type=content_types(name), header="Cache-Control: max-age=3600, must-revalidate")
  }
}
