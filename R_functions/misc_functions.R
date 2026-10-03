

interpolate_colors <- function(colors, weights) {
  if (length(colors) != length(weights)) {
    stop("colors and weights must have the same length")
  }
  
  if (length(colors) == 1) {
    return(colors)
  }
  
  # Normalize weights
  weights <- weights / sum(weights)
  
  # Convert colors to RGB and normalize
  rgb_matrix <- col2rgb(colors) / 255  # This gives a 3 x N matrix
  
  # Weighted sum across columns
  weighted_rgb <- rgb_matrix %*% weights  # Matrix multiplication (3xN) %*% (N)
  
  # Clamp values to [0, 1] to avoid rounding errors
  weighted_rgb <- pmin(pmax(weighted_rgb, 0), 1)
  
  # Convert back to hex color
  rgb(weighted_rgb[1], weighted_rgb[2], weighted_rgb[3])
}


move_to_start <- function(vec, i) {
  if (i < 1 || i > length(vec)) stop("Index out of bounds")
  c(vec[i], vec[-i])
}

move_to_end <- function(vec, i) {
  if (i < 1 || i > length(vec)) stop("Index out of bounds")
  c(vec[-i], vec[i])
}

# https://stackoverflow.com/questions/25631216/r-plots-is-there-a-way-to-draw-a-border-shadow-or-buffer-around-text-labels

shadowtext <- function(x, y=NULL, labels, col='black', bg='white', 
                       theta= seq(0, 2*pi, length.out=50), r=0.1, ... ) {

    xy <- xy.coords(x,y)
    xo <- r*strwidth('A')
    yo <- r*strheight('A')

    # draw background text with small shift in x and y in background colour
    for (i in theta) {
        text( xy$x + cos(i)*xo, xy$y + sin(i)*yo, labels, col=bg, ... )
    }
    # draw actual text in exact xy position in foreground colour
    text(xy$x, xy$y, labels, col=col, ... )
}

draw_circle <- function(r, x = 0, y = 0, ...) {
  x <- seq(-r, r, by = 0.001)
  polygon(c(x, rev(x)), c(sqrt(r^2 - x^2), rev(-sqrt(r^2 - x^2))), ...)
}

prep_latex_variables <- function(named_list) {
  out <- character()
  for (i in 1:length(named_list)) {
    out[i] <- paste0("\\newcommand{\\", names(named_list)[i], "}{", named_list[[i]], "}")
  }
  return(out)
}

dir_init <- function(path, verbose = FALSE) {
    existed <- dir.exists(path)

    unlink(path, recursive = TRUE)
    dir.create(path, recursive = TRUE)

    if (verbose) {
        if (existed) {
            message("Recreated empty directory: ", path)
        } else {
            message("Created directory: ", path)
        }
    }

    invisible(path)
}


HPDI <- function(samples, prob = 0.89) {
    # from rethinking package
    samples <- sort(as.numeric(samples))

    calc_hpdi <- function(p) {
    # coda replacement

        n <- length(samples)
        width <- floor(p * n)

        if (width < 1 || width >= n) {
            stop("prob must define an interval containing between 1 and n - 1 samples")
        }

        lower <- samples[seq_len(n - width)]
        upper <- samples[seq_len(n - width) + width]

        i <- which.min(upper - lower)

        c(lower[i], upper[i])
    }

    x <- sapply(prob, calc_hpdi)

    n <- length(prob)
    result <- numeric(2 * n)

    for (i in seq_len(n)) {

        low_idx <- n + 1 - i
        up_idx <- n + i

        result[low_idx] <- x[1, i]
        result[up_idx] <- x[2, i]

        names(result)[low_idx] <- paste0("|", prob[i])
        names(result)[up_idx] <- paste0(prob[i], "|")
    }

    return(result)
}
