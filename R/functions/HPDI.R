
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
