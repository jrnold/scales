#' Show shapes.
#'
#' A quick and dirty way to show shapes in a plot.
#'
#' @param shapes A numeric or character vector of shapes. See
#' [par()].
#' @param labels Include the plotting character value of the symbol.
#' @family `show_*` functions
#' @return This function is called for the side effect of creating a plot, but
#'   it invisibly returns `shapes`.
#' @importFrom graphics points
#' @export
show_shape <- function(shapes, labels = TRUE) {
  n <- length(shapes)
  ncol <- ceiling(sqrt(n))
  nrow <- ceiling(n / ncol)
  # scale x/y
  m <- 0.5
  x <- c(shapes, rep(NA, nrow * ncol - length(shapes)))
  x <- matrix(x, ncol = ncol, byrow = TRUE)
  x <- x[nrow(x):1, , drop = FALSE]
  plot(0, 0, xlim = c(1 * m, ncol(x) * m), ylim = c(1 * m, nrow(x) * m), type = "n",
       xlab = "", ylab = "", axes = FALSE)
  old <- par(mar = c(0, 0, 0, 0))
  on.exit(par(old))
  for (i in seq_len(ncol(x))) {
    for (j in seq_len(nrow(x))) {
      points(i * m, j * m, pch = x[j, i])
      if (labels) {
        text(i * m, j * m, x[j, i], pos = 1, col = "gray70")
      }
    }
  }
  invisible(shapes)
}

#' Show linetypes
#'
#' A quick and dirty way to show linetypes in a plot.
#'
#' @param linetypes A character vector of linetypes. See [par()] for
#'   linetype values.
#' @param labels Label each line with its linetype (`lty`) value.
#' @family `show_*` functions
#' @return This function called for the side effect of creating a plot.
#'   It returns `linetypes`.
#' @importFrom graphics abline axis
#' @export
show_linetype <- function(linetypes, labels = TRUE) {
  n <- length(linetypes)
  plot(0, 0, xlim = c(0, 1), ylim = c(n, 1), type = "n", xlab = "",
       ylab = "", axes = FALSE)
  for (i in seq_along(linetypes)) {
    abline(h = i, lty = linetypes[i])
  }
  if (labels) {
    axis(side = 2, at = seq_len(n), tick = FALSE, labels = linetypes, las = 2)
  } else {
    axis(side = 2, at = seq_len(n), tick = FALSE, labels = seq_len(n), las = 2)
  }
  invisible(linetypes)
}

#' Show colours.
#'
#' A quick and dirty way to show colours in a plot.
#'
#' @param colours a character vector of colours
#' @param labels boolean, whether to show the hexadecimal representation of the colours in each tile
#' @param borders colour of the borders of the tiles; matches the `border` argument of [graphics::rect()]. The default means `par("fg")`. Use `border = NA` to omit borders.
#' @export
#' @family `show_*` functions
#' @return This function is called for its side-effect, but it invisbly
#'   returns `colours`.
#' @importFrom graphics par plot rect text
show_col <- function(colours, labels = TRUE, borders = NULL) {
  n <- length(colours)
  ncol <- ceiling(sqrt(n))
  nrow <- ceiling(n / ncol)

  x <- c(colours, rep(NA, nrow * ncol - length(colours)))
  x <- matrix(x, ncol = ncol, byrow = TRUE)

  old <- par(pty = "s", mar = c(0, 0, 0, 0))
  on.exit(par(old))

  size <- max(dim(x))
  plot(c(0, size), c(0, -size), type = "n", xlab = "", ylab = "", axes = FALSE)
  rect(col(x) - 1, -row(x) + 1, col(x), -row(x),
       col = x, border = x
  )
  if (labels) {
    text(col(x) - 0.5, -row(x) + 0.5, x)
  }
  invisible(colours)
}
