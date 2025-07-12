

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

logistic <- function(x) exp(x) / (1 + exp(x))

gradient_maker <- function(start=NA, stop=NA, cols=c("darkorange", "white", "darkcyan"), vis=FALSE, n=1000){
    if(is.na(start) | is.na(stop)) stop("need to specify start and stop points on a numerical scale")
    colfunc <- colorRampPalette(cols)
    color.list <- colfunc(n)
    color.locations <- seq(start, stop, length=n)
    names(color.locations) <- color.list
    if(vis==TRUE) plot(color.locations, rep(1, n), col=color.list, pch="|", ylim=c(0.9, 1.1), cex=5)
    return(color.locations)
}

data_gradient <- function(data, colors=c("darkorange", "white", "darkcyan"), my.start=NA, my.stop=NA){
    if(is.na(my.start)) my.start <- min(data, na.rm=TRUE)
    if(is.na(my.stop)) my.stop <- max(data, na.rm=TRUE)
    my.gradient <- gradient_maker(start=my.start, stop=my.stop, cols=colors)
    if(any(data > max(my.gradient), na.rm=T) | any(data < min(my.gradient), na.rm=T)) warning("data is not within gradient range")
    data.colors <- rep(NA, length(data))
    for(i in 1:length(data)){
        if(!is.na(data[i])) data.colors[i] <- names(my.gradient)[which.min(abs(data[i]-my.gradient))]
    }
    data.colors
}

entropy <- function(x, base = exp(1)) {
  p <- prop.table(table(x))
  sum(p * log(1/p, base = base))
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


texttab <- function(input.matrix, alignment = NA,
  hlines = NA, caption = "", scale = NA) {
  output <- character(nrow(input.matrix))
  for (i in 1:nrow(input.matrix)) {
    add.amps <- paste(input.matrix[i, ], collapse = " & ")
    output[i] <- paste(add.amps, "\\\\", sep = " ")
  }
  if (all(!is.na(hlines))) {
    for (i in 1:length(hlines)) {
      output <- append(output, "\\hline", hlines[i] + (i - 1))
    }
  }
  return(output)
}

dir_init <- function(path, verbose = FALSE, overwrite = TRUE) {
  if (substr(path, 1, 2) != "./") stop("path argument must be formatted
    with './' at beginning")
  contents <- dir(path, recursive = TRUE)
  if (dir.exists(path)) {
    if (overwrite) {
      if (verbose) {
        if (length(contents) == 0) print(paste("folder ", path, " created.", sep = ""))
        if (length(contents) > 0) print(paste("folder ", path,
          " wiped of ", length(contents), " files/folders.", sep = ""))
      }
      if (dir.exists(path)) unlink(path, recursive = TRUE)
      dir.create(path)
    }
  } else {
    if (verbose) {
      print(paste("folder ", path, " created.", sep = ""))
    }
    dir.create(path)
  }
}

col_alpha <- function(acol, alpha = 0.2) {
  acol <- col2rgb(acol)
  acol.red <- acol["red", ] / 255
  acol.green <- acol["green", ] / 255
  acol.blue <- acol["blue", ] / 255
  acol <- mapply(
    function(red, green, blue, alphas) {
      rgb(red, green, blue, alphas)
    },
    acol.red, acol.green, acol.blue, alpha
  )
  return(as.character(acol))
}


write_latex_table <- function(df, path, hlines=NA) {
  output <- character(nrow(df))
  for (i in 1:nrow(df)) {
    add.amps <- paste(df[i,], collapse = " & ")
    output[i] <- paste(add.amps, "\\\\", sep = " ")
  }
  if (all(!is.na(hlines))) {
    for (i in seq_along(hlines)) output <- append(output, "\\hline", hlines[i] + (i - 1))
  }
  writeLines(output, path)
}

write_pandoc_table <- function(df, path, style = "rmarkdown", trim_ws = TRUE, ...) {
  df |>
  pandoc.table(style = style, ...) |>
  capture.output() -> out
  if (trim_ws) {
    out <- out[which(out != "")]
  }
  writeLines(out, path)
}
