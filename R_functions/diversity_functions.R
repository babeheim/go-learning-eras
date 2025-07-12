

calc_js_divergence <- function(x, y) {
  moves <- data.frame(
    name = sort(unique(c(x, y))),
    n_x = NA,
    n_y = NA
  )
  for (i in 1:nrow(moves)) {
    moves$n_x[i] = sum(x == moves$name[i])
    moves$n_y[i] = sum(y == moves$name[i])
  }
  moves$prop_x = moves$n_x / sum(moves$n_x)
  moves$prop_y = moves$n_y / sum(moves$n_y)
  out <- JSD(rbind(moves$prop_x, moves$prop_y), unit = "log2")
  return(out)
}

extract_game_nodes <- function(sgf_moves, n_moves) {
  moves <- matrix(NA, nrow = length(sgf_moves), ncol = n_moves)
  sgf_coords <- paste0(rep(letters[1:19], each = 19), rep(letters[1:19], by = 19))
  unicode_symbols <- intToUtf8(0x1F300 + 0:360, multiple = TRUE)  # Starting from U+1F300 (Miscellaneous Symbols)
  for (i in seq_along(sgf_moves)) {
    game_moves <- strsplit(sgf_moves[i], ";")[[1]][1:n_moves]
    blacks <- seq(1, n_moves, 2)
    game_moves[blacks] <- paste0("B", game_moves[blacks])
    game_moves[-blacks] <- paste0("W", game_moves[-blacks])
    out_row <- rep(NA, n_moves)
    out_row[1] <- game_moves[1]
    for (j in 2:n_moves) {
      this_cell <- game_moves[1:j]
      this_cell <- sort(this_cell)
      out_row[j] <- paste(this_cell, collapse = "")
    }
    moves[i,] <- out_row
  }
  return(moves)
}

extract_game_moves <- function(sgf_moves, n_moves, cumulative = FALSE, unicode = FALSE, as_nodes = FALSE) {
  moves <- matrix(NA, nrow = length(sgf_moves), ncol = n_moves)
  sgf_coords <- paste0(rep(letters[1:19], each = 19), rep(letters[1:19], by = 19))
  unicode_symbols <- intToUtf8(0x1F300 + 0:360, multiple = TRUE)  # Starting from U+1F300 (Miscellaneous Symbols)
  for (i in seq_along(sgf_moves)) {
    game_moves <- strsplit(sgf_moves[i], ";")[[1]][1:n_moves]
    if (unicode) game_moves <- unicode_symbols[match(game_moves, sgf_coords)]
    if (cumulative) {
      out_row <- rep(NA, n_moves)
      out_row[1] <- game_moves[1]
      for (j in 2:n_moves) {
        this_cell <- game_moves[1:j]
        if (as_nodes) this_cell <- sort(this_cell)
        out_row[j] <- paste(this_cell, collapse = "")
      }
    } else {
      out_row <- game_moves
    }
    moves[i,] <- out_row
  }
  return(moves)
}

surprisal <- function(x, base = exp(1)) {
  p <- prop.table(table(x))
  s <- log(1/p, base = base)
  s[x]
}

entropy <- function(x, base = exp(1)) {
  p <- prop.table(table(x))
  sum(p * log(1/p, base = base))
}

richness <- function(x) {
  length(unique(x))
}

# entropy(x)
# mean(surprisal(x)) # same as entropy(x)

move_richness <- function(moves) {
  out <- apply(moves, 2, richness)
  out <- as.numeric(out)
  return(out)
}

move_entropy <- function(moves, base) {
  out <- apply(moves, 2, entropy, base = base)
  out <- as.numeric(out)
  return(out)
}

move_surprisals <- function(moves, base = exp(1)) {
  out <- matrix(NA, nrow = nrow(moves), ncol = ncol(moves))
  for (i in seq_len(ncol(moves))) {
    out[,i] <- surprisal(moves[,i])
  }
  out <- as.numeric(out)
  return(out)
}
