.windows_matrices <- function(x, y, z, nw, ntp_in_window) {
    windows <- .windows(x, y, z, nw, ntp_in_window)
    list(mx = matrix(windows$x, ncol = nw),
         my = matrix(windows$y, ncol = nw),
         mz = matrix(windows$z, ncol = nw))
}

#' @param nw the number of windows
#' @param ntp_in_window the number of time points in a window
#' @param bin_width Hz, the interval between the reference points in the bispectral domain
#' @param sampling_rate Hz, the sampling rate
#' @param F1 Hz, the frequency of interest of the first spectral axis
#' @param F2 Hz, the frequency of interest of the second spectral axis
#'
#' @importFrom dplyr case_when filter if_else mutate pull
#' @importFrom rhosa cross_bicoherence mode_matching
#' @importFrom tibble add_column as_tibble
.bqpc <- function(x, y, z, nw, ntp_in_window, bin_width, sampling_rate, F1, F2) {
    wm <- .windows_matrices(x, y, z, nw, ntp_in_window)

    Q <- case_when((F1 + F2) <= (sampling_rate / 2) ~ 'I',
                   F1 > F2 ~ 'II',
                   TRUE ~ 'III')
    Fx <- F1
    Fy <- case_when(Q == 'I' ~ F2,
                    TRUE ~ -F2)

    xbc <- cross_bicoherence(wm$mx, wm$my, wm$mz)
    if (bin_width != sampling_rate / ntp_in_window) {
        xbc <- place_xbc_in_grid(xbc, bin_width, sampling_rate)
    }
    mm <- mode_matching(xbc)

    xbc_Hz <- as_tibble(xbc) %>%
        mutate(f1_Hz = f1 * sampling_rate,
               f2_Hz = f2 * sampling_rate) %>%
        add_column(significance = if_else(mm$fdr < 0.05, "Y", "N"))

    ## introduce a small margin around the ROI to avoid the issue of rounding floating-point numbers
    margin <- .delta / (ntp_in_window * sampling_rate)

    xbc_Hz_Q <- xbc_Hz %>%
        filter(case_when(Q == 'I' ~ (f1 > 0 & f2 > 0),
                         Q == 'II' ~ ((f1 + f2) > 0 & f2 < 0),
                         TRUE ~ (f1 + f2) < 0)) %>%
        mutate(roi = if_else(between(f1_Hz, Fx - .delta/2 - margin, Fx + .delta/2 + margin) & between(f2_Hz, Fy - .delta/2 - margin, Fy + .delta/2 + margin), 'in', 'out'))

    xbc_sum_sig <- xbc_Hz_Q %>%
        filter(significance == 'Y') %>%
        pull(value) %>%
        sum()

    xbc_sum_sig_roi <- xbc_Hz_Q %>%
        filter(significance == 'Y', roi == 'in') %>%
        pull(value) %>%
        sum()

    xbc_volume_sig <- xbc_sum_sig * (bin_width^2)

    xbc_volume_sig_roi <- xbc_sum_sig_roi * (bin_width^2)

    R_BQPC <- if_else(xbc_sum_sig > 0, xbc_sum_sig_roi / xbc_sum_sig, 0)

    list(Q = Q,
         mm = mm,
         xbc_Hz = xbc_Hz,
         xbc_Hz_Q = xbc_Hz_Q,
         xbc_volume_sig = xbc_volume_sig,
         xbc_volume_sig_roi = xbc_volume_sig_roi,
         R_BQPC = R_BQPC)
}
