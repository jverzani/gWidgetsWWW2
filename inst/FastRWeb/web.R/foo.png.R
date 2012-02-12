    # foo.png.R:
    run <- function(n=100, ...) {
      n <- as.integer(n)
      p <- WebPlot(800, 600)
      plot(rnorm(n), rnorm(n), pch=19, col=2)
      p
    }
