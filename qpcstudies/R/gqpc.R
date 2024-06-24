#' @importFrom purrr map_dfr
.windows <- function(x, y, z, nw, ntp_in_window) {
    map_dfr(seq_len(nw), function(k) {
        i_begin <- (ntp_in_window/2) * (k - 1) + 1
        i_end <- (ntp_in_window/2) * (k + 1)
        data.frame(k = k,
                   x = x[i_begin:i_end],
                   y = y[i_begin:i_end],
                   z = z[i_begin:i_end])
    })
}

#' @importFrom vars VARselect
.select_lag_order <- function(data) {
    xy <- data$x * data$y
    z <- data$z
    xyz <- data.frame(z = z, xy = xy)
    vs <- VARselect(xyz, lag.max = 50, type = 'both')

    BIC <- 3 # just for the named index
    vcBIC <- vs$criteria[BIC,]
    r <- ifelse(min(vcBIC) >= 0, 1.05, 0.95)
    threshold <- min(vcBIC) * r
    which(vcBIC <= threshold)[1]
}

#' @importFrom dplyr mutate nest_by
#' @importFrom magrittr %>%
.determine_p <- function(windows) {
    k_data_p <- windows %>%
        nest_by(k) %>%
        mutate(p = .select_lag_order(data))

    max(k_data_p$p)
}

#' @param data the data frame of time series x, y, and z
#' @param p the lag order
#' @param nboots the number of bootstraps
#' 
#' @importFrom grangers Granger.unconditional
#' @importFrom qvalue empPvals qvalue
.ugcqval <- function(data, p, nboots) {
    cat(sprintf('%s ', Sys.time()))

    xy <- data$x * data$y
    z <- data$z
    xyz <- data.frame(z = z, xy = xy)

    ugc <- Granger.unconditional(z, xy, type.chosen = 'both', p = p)
    ugctest <- .Granger.inference.unconditional(z, xy, type.chosen = 'both', p = p, nboots = nboots)

    p_value <- empPvals(stat = ugc$Unconditional_causality_y.to.x, stat0 = ugctest$p_y.to.x)
    q_value <- tryCatch({
        qvalue(p = p_value, pfdr = TRUE)
    }, error = function(e) {
        qvalue(p = p_value, pfdr = TRUE, lambda = 0)
    })

    ugcqval <- data.frame(frequency = ugc$frequency * .sampling_rate,
                          value = ugc$Unconditional_causality_y.to.x,
                          qvalues = q_value$qvalues)

    attr(ugcqval, "limit") <- .ugclim(ugcqval)

    cat(sprintf('threshold=%f\n', attr(ugcqval, "limit")))

    ugcqval
}

#' @importFrom dplyr filter pull
.ugclim <- function(ugcqval) {
    ugcqval %>%
        filter(qvalues < 0.05) %>%
        pull(value) %>%
        min()
}

.madm <- function(v) {
    median(abs(v - median(v)))/sqrt(length(v))
}

#' @param sampling_rate Hz, the sampling rate
#' @param F1 Hz, the frequency of interest of the first spectral axis
#' @param F2 Hz, the frequency of interest of the second spectral axis
#'
#' @importFrom dplyr between filter if_else pull select
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @importFrom purrr map_dfc
#' @importFrom vars VARselect
.gqpc <- function(x, y, z, nw, ntp_in_window, sampling_rate, F1, F2) {
    nboots <- as.integer(sqrt(sampling_rate/ntp_in_window) * 1000)

    set.seed(42)

    windows <- .windows(x, y, z, nw, ntp_in_window)

    p <- .determine_p(windows)

    all_results <- Map(function(.k) {
        data <- windows |>
            filter(k == .k) |>
            select(!k)
        .ugcqval(data, p, nboots)
    }, seq_len(nw))

    results <- Filter(function(x) {is.finite(attr(x, "limit"))}, all_results)

    ## frequency (row) x window (column)
    value_matrix <- as.matrix(map_dfc(results, function(x) {x$value}))

    frequency <- results[[1]]$frequency

    ## introduce a small margin around the ROI to avoid the issue of rounding floating-point numbers
    margin <- .delta / (ntp_in_window * sampling_rate)

    in_roi <- between(frequency, (F1 + F2) - .delta - margin, (F1 + F2) + .delta + margin) |
        between(frequency, (.sampling_rate - (F1 + F2)) - .delta - margin, (.sampling_rate - (F1 + F2)) + .delta + margin)

    value_m <- apply(value_matrix, 1, median)
    value_madm <- apply(value_matrix, 1, .madm)

    median_limit <- median(sapply(results, function(x) {attr(x, "limit")}))

    median_ugc <- tibble(frequency = frequency,
                         value = value_m,
                         value_plus_2madm = value_m + 2 * value_madm,
                         value_minus_2madm = value_m - 2 * value_madm,
                         significance = if_else(median_limit <= value_m, 'Y', 'N'),
                         roi = if_else(in_roi, 'in', 'out'))

    median_ugc_sum_sig <- median_ugc %>%
        filter(significance == 'Y') %>%
        pull(value) %>%
        sum()

    median_ugc_sum_sig_roi <- median_ugc %>%
        filter(significance == 'Y', roi == 'in') %>%
        pull(value) %>%
        sum()

    median_ugc_area_sig <- median_ugc_sum_sig * (.sampling_rate / ntp_in_window)
    median_ugc_area_sig_roi <- median_ugc_sum_sig_roi * (.sampling_rate / ntp_in_window)

    R_GQPC <- if_else(median_ugc_sum_sig > 0, median_ugc_sum_sig_roi / median_ugc_sum_sig, 0)

    list(p = p,
         median_limit = median_limit,
         median_ugc = median_ugc,
         median_ugc_area_sig = median_ugc_area_sig,
         median_ugc_area_sig_roi = median_ugc_area_sig_roi,
         R_GQPC = R_GQPC)
}
