w <- gwindow("checkbox")
g <- ggroup(cont=w, horizontal=FALSE)
x <- state.name[1:4]

##
## checkbox
##

## vanilla
cb1 <- gcheckbox(x[1], cont=g)

## with handler
cb2 <- gcheckbox(x[1], cont=g, handler=function(h,...) print(svalue(h$obj)))

## add handler
cb3 <- gcheckbox(x[1], cont=g)
addHandlerChanged(cb3, handler=function(h,...) print("add handler"))

## initially checked
cb4 <- gcheckbox(x[1], checked=TRUE, cont=g)

## tests

## svalue
expect_equal(svalue(cb1), FALSE)
expect_equal(svalue(cb4), TRUE)

## svalue <-
svalue(cb1) <- TRUE
expect_equal(svalue(cb1), TRUE)
expect_error(svalue(cb1) <- "not logical")


## [
expect_equal(cb1[], x[1])

## [<-
expect_warning(cb1[] <- "new label")


## invoke handler
expect_output(cbg3$invoke_change_handler(), "add handler")


##
## checkboxgroup
##

## vanilla
cbg1 <- gcheckboxgroup(x, cont=g)

## with handler
cbg2 <- gcheckboxgroup(x, cont=g, handler=function(h,...) print(svalue(h$obj)))

## add handler
cbg3 <- gcheckboxgroup(x, cont=g)
addHandlerChanged(cbg3, handler=function(h,...) print("add handler"))

## initial values
cbg4 <- gcheckboxgroup(x, checked=c(TRUE, FALSE, TRUE, FALSE), cont=g)



## svalue
expect_equal(svalue(cbg1), character())
expect_equal(svalue(cbg4), x[c(TRUE, FALSE, TRUE, FALSE)])

## svalue <-
## by name
svalue(cbg1) <- x[2]
expect_equal(svalue(cbg1), x[2])

## by index
svalue(cbg1, index=TRUE) <- 1:3
expect_equal(svalue(cbg1), x[1:3])

## by logical
svalue(cbg1) <- c(TRUE, FALSE, TRUE, FALSE)
expect_equal(svalue(cbg1), x[c(1,3)])

## [
expect_equal(cbg1[], x)



## invoke handler
expect_output(cbg3$invoke_change_handler(), "add handler")
