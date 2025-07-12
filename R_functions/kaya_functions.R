
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


validate_sgfs_fast <- function (files, num_cores = 3) {
    output <- rep("", length(files))
    if (length(files) == 1) {
        if (!file.exists(files)) 
            stop("no file at this location")
        res <- try(game_data <- read_sgf(files), silent = TRUE)
        failed <- class(res) == "try-error"
        if (failed) {
            output <- paste(files, "is not a valid sgf file")
        }
        else {
            coords <- as.character(game_data$moves$coord_sgf)
            coords_invalid <- !all(unlist(strsplit(coords, "")) %in% 
                letters[1:20])
            coords_wronglength <- !all(nchar(coords) %in% c(0, 
                2))
            if (coords_invalid | coords_wronglength) {
                output <- "coordinates are invalid"
            }
            duplicate_key <- any(duplicated(names(game_data)))
            long_keys <- unlist(lapply(game_data, length) > 1)
            long_keys <- names(long_keys[long_keys])
            long_key_error <- !all(long_keys %in% c("moves", 
                "AB", "AW"))
            if (duplicate_key | long_key_error) {
                if (output != "") 
                  output <- paste(output, "sgf codes are invalid", 
                    sep = "; ")
                if (output == "") 
                  output <- "sgf codes are invalid"
            }
            if (!(long_key_error | duplicate_key | coords_wronglength | 
                coords_invalid)) {
                output <- "sgf is valid"
            }
        }
    }
    else {
      output <- mclapply(seq_along(files), function(z) {
        if (z %% 100 == 0) print(z)
        validate_sgfs_fast(files[z])
        }, mc.cores = num_cores)
    }
    return(output)
}

create_database_fast <- function(sgf_paths, num_cores = 3) {
  data_list <- parallel::mclapply(seq_along(sgf_paths), function(z) {
    game_data <- read_sgf(sgf_paths[z], rotate = FALSE)
    if (class(game_data) != "try-error") {
      game_data$m1 <- game_data$moves$coord_sgf[1]
      game_data$m2 <- game_data$moves$coord_sgf[2]
      game_data$filename <- sgf_paths[z]
      game_data <- game_data[-which(names(game_data) %in% c("AB", "AW", "moves"))]
      if (z %% 100 == 0) print(z)
      return(game_data)
    }
  }, mc.cores = num_cores)
  output <- as.data.frame(bind_rows(data_list))
  return(output)
}

create_database_opening <- function(sgf_paths, num_cores = 3) {
  data_list <- parallel::mclapply(seq_along(sgf_paths), function(z) {
    game_data <- read_sgf(sgf_paths[z], rotate = TRUE)
    if (class(game_data) != "try-error") {
      game_data$m1 <- game_data$moves$coord_sgf[1]
      game_data$m2 <- game_data$moves$coord_sgf[2]
      game_data$opening <- paste(game_data$moves$coord_sgf[1:50], collapse = ";")
      game_data$filename <- sgf_paths[z]
      game_data <- game_data[-which(names(game_data) %in% c("AB", "AW", "moves"))]
      if (z %% 100 == 0) print(z)
      return(game_data)
    }
  }, mc.cores = num_cores)
  output <- as.data.frame(bind_rows(data_list))
  return(output)
}
