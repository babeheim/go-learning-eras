
sgf_to_korschelt <- function(x) {
  if (length(x) == 1) {
    if (nchar(x) == 2) {
      xs <- strsplit(x, "")[[1]]
      sgfs <- letters[1:19]
      cols <- setdiff(LETTERS[1:20], "I")
      out <- c(cols[match(xs[1], sgfs)], match(xs[2], rev(sgfs)))
      out <- paste(out, collapse = "")
    } else if (grepl("^([A-Za-z]{2};)*$", x)){
      y <- strsplit(x, ";")[[1]]
      for (i in 1:length(y)) y[i] <- sgf_to_korschelt(y[i])
      out <- paste(y, collapse = ",")
    } else {
      warning("input not recognized")
    }
  } else {
    out <- rep(NA, length(x)) 
    for (i in 1:length(x)) out[i] <- sgf_to_korschelt(x[i])
  }
  return(out)
}
