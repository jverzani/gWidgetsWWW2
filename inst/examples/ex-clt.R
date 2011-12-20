## Example of a CLT demonstration.

f <- get_tempfile()                         # gcanvas uses file, not url

makePlot <- function(h,...) {
  n <- min(as.numeric(svalue(sampleSize)), 500)
  m <- min(as.numeric(svalue(replicates)), 500)
  cmd <- pops[[svalue(population, index=TRUE)]]
  cmd$n <- n * m
  x <- matrix(do.call(cmd$cmd, cmd[-(1:2)]), nrow=n)
  xbars <- apply(x, 2, function(x) mean(x))
  canvas(f, width=width, height=height, bg="#ffffff")
  plot(density(xbars), main=sprintf("Population: %s, n: %s", svalue(population), svalue(sampleSize)))
  rug(xbars)
  dev.off()
  svalue(p) <- f
}


## Begin layout
w <- gwindow("CLT example")
gstatusbar("Powered by gWidgetsWWWs and Rook", cont = w)


g <- ggroup(cont=w, horizontal=FALSE)

l <- glabel("Distribution of xbar, the sample mean.", cont = g)

pg <- gpanedgroup(height=400, cont=g, default.size = 300)

## main widget is canvas
width <- height <- 400                  # canvas size
p <- gcanvas(width=width, height=height, cont = pg)
fg <- ggroup(horizontal=FALSE, cont=pg)

pops <- list("rnorm(n,0,1)" = list(cmd="rnorm", tooltip="Normal", mean=0, sd=1),
             "rexp(n)" = list(cmd="rexp", tooltip="Skewed"),
             "rt(n, df=3)" = list(cmd="rt",tooltip="Symmetric, long tailed", df=3),
             "rt(n, df=30)" = list(cmd="rt", tooltip="Symmetric, some tail", df=30),
             "runif(n)" = list(cmd="runif", tooltip="Short tail"),
             "rlnorm(n)" = list(cmd="rlnorm", tooltip="Very skewed")
             )
tbl <- glayout(cont = fg)
tbl[1,1] <- "n (sample size)"
tbl[1,2] <- (sampleSize <- gedit("10", cont = tbl, coerce.with="as.numeric"))

tbl[2,1] <- "Population:"
tbl[2,2] <- (population <- gcombobox(names(pops), width="auto",
                                     editable=FALSE, cont=tbl))
tbl[3,1] <- "Replicates"
tbl[3,2] <- (replicates <- gslider(from=10, to=200, by=1, value=50, cont=tbl))
visible(tbl) <- TRUE

g1 <- ggroup(cont = fg)
b <- gbutton("plot", cont = g1, handler=makePlot)
helpButton <- gbutton("help", cont = g1, handler=function(h,...) {
  w1 <- gwindow("Help with CLT demo", width=400, height=300, parent=w)
  g <- ggroup(cont = w1, horizontal=FALSE)
  ghtml(paste("<h1>CLT Example</h1>",
              "Demonstrates the Central Limit Theorem, which says the CDF of the sample mean",
              "of a random sample",
              "of <i>n</i> data points converges to that of the normal distribution as",
              "<i>n</i> goes to infinity. This shows a density estimate, so it is not quite the same for discrete",
              "populations, but still illustrates the point.",
              "<p>",
              "<UL>",
              "<li>Set the <b>sample size</b> by adjusting the value of <i>n</i>.</li>",
              "<li>Set the <b>population</b> from one of the set ones.",
              "The value <i>n</i>can be used as a parameter.</li>",
              "<li>Adjust the number of <b>replicates</b> if desired.</li>",
                "</UL>",
              "Click the <b>plot</b> button to update the plot",
              sep=" "), cont = g, expand=TRUE)
  gseparator(cont = g)
  gbutton("dismiss", cont = g, handler=function(h,...) {
    dispose(w1)
  })
  visible(w1) <- TRUE
})



makePlot(1)



