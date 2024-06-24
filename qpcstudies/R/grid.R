#### Copied from rhosa's cross_bispectra.R
#### /BEGIN

## Return a data frame of frequency pairs in the 1st quadrant.
## The unit of frequency x1 and x2 is cycles.
##
## @param n the number of samples
.generate_1st_quadrant <- function(n) {
    stopifnot(length(n) == 1 && n >= 1)

    if (n < 4) {
        data.frame(x1 = integer(), x2 = integer())
    } else {
        ymax <- function(x) n %/% 2 - x
        xs <- seq_len(n %/% 2 - 1)
        do.call(rbind, Map(function(x, u) data.frame(x1 = x, x2 = 1:u), xs, ymax(xs)))
    }
}

## Return a data frame of frequency pairs in the 4th quadrant.
## The x2 is positive, yet represents a negative frequency of the same absolute value.
## Note that it excludes the row of x1 == x2, at which the third frequency is zero.
##
## @inheritParams .generate_1st_quadrant
.generate_4th_quadrant <- function(n) {
    stopifnot(length(n) == 1 && n >= 1)

    if (n < 4) {
        data.frame(x1 = integer(), x2 = integer())
    } else {
        xs <- seq_len(n %/% 2 - 1)
        do.call(rbind, Map(function(x, u) expand.grid(x1 = x, x2 = setdiff(xs, x)), xs))
    }
}

#### /END

#' @param bin_width in Hz, the interval between adjacent reference points of bifrequencies
#' @param sampling_rate in Hz
#' @export
generate_grid <- function(bin_width, sampling_rate) {
    stopifnot(bin_width > 0,
              sampling_rate > 0,
              bin_width < sampling_rate/2)
    n <- sampling_rate %/% bin_width
    q1 <- .generate_1st_quadrant(n)
    q4 <- .generate_4th_quadrant(n)
    data.frame(f1 = c(q1$x1, q4$x1) * bin_width / sampling_rate,
               f2 = c(q1$x2, -q4$x2) * bin_width / sampling_rate)
}

#' @param xbc a data frame of cross-bicoherence values e.g. returned from cross_bicoherence()
#' @inheritParams generate_grid
#' @export
place_xbc_in_grid <- function(xbc, bin_width, sampling_rate) {
    g <- generate_grid(bin_width, sampling_rate)
    r <- bin_width/2 # radius of the surrounding square of a reference point
    gv <- function(.f1, .f2) {
        xbc |>
            dplyr::filter(abs(f1 - .f1) * sampling_rate <= r,
                          abs(f2 - .f2) * sampling_rate <= r) |>
            dplyr::pull(value) |>
            mean()
    }
    data.frame(f1 = g$f1,
               f2 = g$f2,
               value = purrr::map2_dbl(g$f1, g$f2, gv))
}
