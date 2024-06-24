.avg_pow <- function(v) {
    mean(v^2)
}

.snr <- function(s, n) {
    .avg_pow(s)/.avg_pow(n)
}

.snrA <- function(Q) {
    ((2 * pi^2 + 6) * Q^2) / 3 + 1/2
}

.snrB <- function(Q) {
    (pi^2 * Q^2) / 3 + 1/2
}

.snrC <- function(Q) {
    ((pi^2 * Q^2) / 3 + 1/2) / ((pi^2 * Q^2) / 3 + 2 * Q^2 + 1)
}
