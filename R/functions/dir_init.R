
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
