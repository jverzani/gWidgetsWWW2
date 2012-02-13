##' code to handle file uploads
##' This is Rook:::Multipart$parse except for one *minor change of input$read(1L) to shift input by 1 byte
parse = function(env){
  EOL <- "\r\n"
  
  if (!exists('CONTENT_TYPE',env)) return(NULL)
  
                                        # return value
  params  <- list()
  
  input <- env$`rook.input`
  input$rewind()
  
  content_length = as.integer(env$CONTENT_LENGTH)
  
                                        # Some constants regarding boundaries
  boundary <- paste('--',gsub('^multipart/.*boundary="?([^";,]+)"?','\\1',env$CONTENT_TYPE,perl=TRUE),sep='')
  boundary_size <- Rook:::Utils$bytesize(boundary)
  boundaryEOL <- paste(boundary,EOL,sep='')
  boundaryEOL_size <- boundary_size + Rook:::Utils$bytesize(EOL)
  EOLEOL = paste(EOL,EOL,sep='')
  EOLEOL_size = Rook:::Utils$bytesize(EOLEOL)
  EOL_size = Rook:::Utils$bytesize(EOL)
  
  buf <- new.env()
  buf$bufsize <- 16384 # Never read more than bufsize bytes.
  input$read(1L)       # XXX WHY??
  buf$read_buffer <- input$read(boundaryEOL_size)
  buf$read_buffer_len <- length(buf$read_buffer)
  buf$unread <- content_length - boundary_size
  
  i <- Utils$raw.match(boundaryEOL,buf$read_buffer,all=FALSE)
  if (!length(i) || i != 1){
    warning("bad content body")
    input$rewind()
    return(NULL)
  }
  
  fill_buffer <- function(x){
    buf <- input$read(ifelse(x$bufsize < x$unread, x$bufsize, x$unread))
    buflen <- length(buf)
    if (buflen > 0){
      x$read_buffer <- c(x$read_buffer,buf)
      x$read_buffer_len <- length(x$read_buffer)
      x$unread <- x$unread - buflen
    }
  }
  
                                        # Slices off the beginning part of read_buffer.
  slice_buffer <- function(i,size,x){
    slice <- if(i > 1) x$read_buffer[1:(i-1)] else x$read_buffer[1] 
    x$read_buffer <- if(size < x$read_buffer_len) x$read_buffer[(i+size):x$read_buffer_len] else raw()
    x$read_buffer_len <- length(x$read_buffer)
    slice
  }
  
                                        # prime the read_buffer
  buf$read_buffer <- raw()
  fill_buffer(buf)
  
  while(TRUE) {
    head <- value <- NULL
    filename <- content_type <- name <- NULL
    while(is.null(head)){
      i <- Utils$raw.match(EOLEOL,buf$read_buffer,all=FALSE)
      if (length(i)){
        head <- slice_buffer(i,EOLEOL_size,buf)
        break
      } else if (buf$unread){
        fill_buffer(buf)
      } else {
        break # we've read everything and still haven't seen a valid head
      }
    }
    if (is.null(head)){
      warning("Bad post payload: searching for a header")
      input$rewind()
      return(NULL)
    } 
    
                                        # cat("Head:",rawToChar(head),"\n")
                                        # they're 8bit clean
    head <- rawToChar(head)
    
    token <- '[^\\s()<>,;:\\"\\/\\[\\]?=]+'
    condisp <- paste('Content-Disposition:\\s*',token,'\\s*',sep='')
    dispparm <- paste(';\\s*(',token,')=("(?:\\"|[^"])*"|',token,')*',sep='')
    
    rfc2183 <- paste('(?m)^',condisp,'(',dispparm,')+$',sep='')
    broken_quoted <- paste('(?m)^',condisp,'.*;\\sfilename="(.*?)"(?:\\s*$|\\s*;\\s*',token,'=)',sep='')
    broken_unquoted = paste('(?m)^',condisp,'.*;\\sfilename=(',token,')',sep='')
    
    if (length(grep(rfc2183,head,perl=TRUE))){
      first_line <- sub(condisp,'',strsplit(head,'\r\n')[[1L]][1],perl=TRUE)
      pairs <- strsplit(first_line,';',fixed=TRUE)[[1L]]
      fnmatch <- '\\s*filename=(.*)\\s*'
      if (any(grepl(fnmatch,pairs,perl=TRUE))){
        filename <- pairs[grepl(fnmatch,pairs,perl=TRUE)][1]
        filename <- gsub('"','',sub(fnmatch,'\\1',filename,perl=TRUE))
      }
    } else if (length(grep(broken_quoted,head,perl=TRUE))){
      filename <- sub(broken_quoted,'\\1',strsplit(head,'\r\n')[[1L]][1],perl=TRUE)
    } else if (length(grep(broken_unquoted,head,perl=TRUE))){
      filename <- sub(broken_unquoted,'\\1',strsplit(head,'\r\n')[[1L]][1],perl=TRUE)
    }

    if (!is.null(filename) && filename!=''){
      filename = Utils$unescape(filename)
    }

    headlines <- strsplit(head,EOL,fixed=TRUE)[[1L]]
    content_type_re <- '(?mi)Content-Type: (.*)'
    content_types <- headlines[grepl(content_type_re,headlines,perl=TRUE)]
    if (length(content_types)){
      content_type <- sub(content_type_re,'\\1',content_types[1],perl=TRUE)
    }

    name <- sub('(?si)Content-Disposition:.*\\s+name="?([^";]*).*"?','\\1',head,perl=TRUE)

    while(TRUE){
      i <- Utils$raw.match(boundary,buf$read_buffer,all=FALSE)
      if (length(i)){
        value <- slice_buffer(i,boundary_size,buf)
        if (length(value)){

                                        # Drop EOL only values
          if (length(value) == 2 && length(Utils$raw.match(EOL,value)))
            break

          if (!is.null(filename) || !is.null(content_type)){
            data <- list()
            if (!is.null(filename))
              data$filename <- strsplit(filename,'[\\/]',perl=TRUE)[[1L]]
            data$tempfile <- tempfile('Multipart')
            if (!is.null(content_type))
              data$content_type <- content_type
            data$head <- head
            con <- file(data$tempfile,open='wb')
            writeBin(value,con)
            close(con)
            params[[name]] <- data
          } else {
            len <- length(value)
                                        # Trim trailing EOL
            if (len > 2 && length(Utils$raw.match(EOL,value[(len-1):len],all=FALSE)))
              len <- len -2
            params[[name]] <- Utils$escape(rawToChar(value[1:len]))
          }
        } 
        break
      } else if (buf$unread){
        fill_buffer(buf)
      } else {
        break # we've read everything and still haven't seen a valid value
      }
    }
    if (is.null(value)){
                                        # bad post payload
      input$rewind()
      warning("Bad post payload: searching for a body part")
      return(NULL)
    }

                                        # Now search for ending markers or the beginning of another part
    while (buf$read_buffer_len < 2 && buf$unread) fill_buffer(buf)

    if (buf$read_buffer_len < 2 && buf$unread == 0){
                                        # Bad stuff at the end. just return what we've got
                                        # and presume everything is okay.
      input$rewind()
      return(params)
    }

                                        # Valid ending
    if (length(Utils$raw.match('--',buf$read_buffer[1:2],all=FALSE))){
      input$rewind()
      return(params)
    } 
                                        # Skip past the EOL.
    if (length(Utils$raw.match(EOL,buf$read_buffer[1:EOL_size],all=FALSE))){
      slice_buffer(1,EOL_size,buf)
    } else {
      warning("Bad post body: EOL not present")
      input$rewind()
      return(params)
    }

                                        # another sanity check before we try to parse another part
    if ((buf$read_buffer_len + buf$unread) < boundary_size){
      warning("Bad post body: unknown trailing bytes")
      input$rewind()
      return(params)
    }
  }
}

##' Find file in the post body. Heavy lifting done by Rook function Multipart$parse with minor modification
run <- function(...) {

  env <- new.env()
  env$CONTENT_TYPE <- request$c.type
  env$CONTENT_LENGTH <- request$c.length
  env$rook.input <- Rook:::RhttpdInputStream$new(rawToChar(request$body))

  mp <-parse(env)

  if(!is.null(mp)) {
    session_id <- mp$session_id; mp$session_id <- NULL
    id <- mp$id; mp$id <- NULL
    if(length(mp)) {
      mp <- mp[[1]]                       # has funny name
      e <- session_manager$get_session_by_id(session_id)
      on.exit(session_manager$store_session(session_id, e))
      toplevel <- e[[".gWidgets_toplevel"]]
      obj <- toplevel$get_object_by_id(id)
      
      fname <- mp$tempfile
      orig_name <- mp$filename
      
      if(file.exists(fname)) {
        obj$set_value(fname)
        obj$set_filename(orig_name)
        out("{success:true}")
      } else {
        out("{success:false}")          # no expecting this
      }
    } else {
      out("{success:false}")   # file removed can cause this
    }
  } else {
    out("{success:false}")    
  }
  done()
  
}
