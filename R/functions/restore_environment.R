restore_environment <- function(project = ".") {

    if (!requireNamespace("renv", quietly = TRUE)) {
        install.packages("renv")
    }

    renv::restore(
        project = project,
        prompt = FALSE
    )

    invisible(TRUE)
}
