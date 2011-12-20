## An example of actions
## gWidgets uses actions for toolbar, menubars and buttons
## this shows how actions can be enabled/disabled

w <- gwindow("Example of using actions")
g <- ggroup(cont=w, horizontal=FALSE)
l <- glabel("An example of how to manipulate actions", cont=g)
l <- glabel("(icons aren't yet hooked up)", cont=g) 

handler <- function(h,...) gmessage("called handler", parent=w)

## list of actions.
## actions need the parent argument in gWidgetsWWW, not so in other
## gWidgets
alist = list(
  new = gaction(label="new",icon="new",handler = handler, parent = w),
  open = gaction(label="open",icon="open",handler = handler, parent = w),
  save = gaction(label="save",icon="save",handler = handler, parent = w),
  save.as = gaction(label="save as...",icon="save as...",handler = handler, parent = w),
  quit = gaction(label="quit",icon="quit",handler = handler, parent = w),
  cut = gaction(label="cut",icon="cut",handler = handler, parent = w)
  )

## menu bar list
mlist <- list(file = list(
                new = alist$new,
                open = alist$open,
                save = alist$save,
                "save as..." = alist$save.as,
                quit = alist$quit
                ),
              edit = list(
                cut = alist$cut
                )
              )

mb <- gmenu(mlist, cont=w)
l <- glabel("Buttons can take actions too.", cont=g)
b <- gbutton(action = alist$save, cont = g)
gseparator(cont=g)
b1 <- gbutton("set actions as if a \"no changes yet\" state", cont =g,
              handler = function(h,...) {
                nms <- names(alist)
                grayThese <- c("save","save.as","cut")
                for(i in grayThese)
                  enabled(alist[[i]]) <- FALSE
                for(i in setdiff(nms, grayThese))
                  enabled(alist[[i]]) <- TRUE
              })

b2 <- gbutton("set actions as if \"some change\" is the  state", cont =g,
              handler = function(h,...) {
                nms <- names(alist)
                grayThese <- c()
                for(i in grayThese)
                  enabled(alist[[i]]) <- FALSE
                for(i in setdiff(nms, grayThese))
                  enabled(alist[[i]]) <- TRUE
              })

## show the window
gstatusbar("Powered by RApache and gWidgetsWWW", cont = w)
visible(w) <- TRUE


