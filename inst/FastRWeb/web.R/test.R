

run <- function(id, ...) {
  fname <- sprintf("/tmp/testing/%s", id)

  if(file.exists(fname)) {
    con <- file(fname, "r")
    e <- unserialize(con)
    close(con)
  } else {
    e <- new.env()
    e[['a']] <- 1
  }

  e[['a']] <- e[['a']] + 1

  con <- file(fname, "w")
  serialize(e, con)
  close(con)

  out(e[['a']])
#  out("All working")
  done()

  
}
