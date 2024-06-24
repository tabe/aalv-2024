.study0_simulation_length <- 500 # the number of time points in a simulated sample (i.e. epoch)

.study0_sample_size <- 128 # the number of simulated samples (i.e. epochs)

.study0_ntp <- .study0_simulation_length * .study0_sample_size # the total number of time points in given input time series

.study0_ntp_in_window <- 500 # the number of time points in a window

.study0_nw <- 255 # the total number of windows

stopifnot((.study0_ntp_in_window/2) * (.study0_nw+1) <= .study0_ntp)

#' @import ggplot2
#' @importFrom purrr map_dfr
#' @importFrom dplyr bind_cols mutate
#' @importFrom readr read_csv
.plot_mvgc <- function(args, gf) {
    if (length(args) < 1)
        stop('missing arguments')

    output_filename <- args[1]

    data2 <- read_csv("conf.csv", show_col_types = FALSE) %>%
        mutate(cols = sprintf('F1 = %g\nF2 = %g\nF3 = %g', Fcoef1, Fcoef2, Fcoef3),
                      rows = sprintf('Q = %g', Qcoef))
    data1 <- map_dfr(seq_len(nrow(data2)), function(i) {
        csv_filename <- sprintf('%s/mvgc.csv', data2$name[i])
        read_csv(csv_filename, col_names = c('frequency', 'value'), show_col_types = FALSE) %>%
            bind_cols(data2[i,])
    })

    p <- ggplot(data2) +
        geom_vline(aes(xintercept = Fcoef1), colour = 'red', linewidth = 0.3) +
        geom_text(aes(x = Fcoef1), y = Inf, vjust = 'inward', label = 'F1', colour = 'red') +
        geom_vline(aes(xintercept = Fcoef2), colour = 'green', linewidth = 0.3) +
        geom_text(aes(x = Fcoef2), y = Inf, vjust = 'inward', label = 'F2', colour = 'green') +
        geom_vline(aes(xintercept = Fcoef3), colour = 'blue', linewidth = 0.3) +
        geom_text(aes(x = Fcoef3), y = Inf, vjust = 'inward', label = 'F3', colour = 'blue') +
        geom_vline(aes(xintercept = Fcoef1 + Fcoef2), colour = 'pink', linewidth = 0.3) +
        geom_text(aes(x = Fcoef1 + Fcoef2), y = Inf, vjust = 'inward', label = 'F1 + F2', colour = 'pink', size = 2) +
        geom_vline(aes(xintercept = 100 - (Fcoef1 + Fcoef2)), colour = 'pink', linewidth = 0.3) +
        geom_text(aes(x = 100 - (Fcoef1 + Fcoef2)), y = Inf, vjust = 'inward', label = '100 - (F1 + F2)', colour = 'pink', size = 2) +
        gf(aes(x = frequency, y = value), data = data1) +
        coord_cartesian(xlim = c(0, .sampling_rate/2)) +
        xlab('Frequency [Hz]') +
        ylab('Unconditional GC of X3 from X1 * X2') +
        labs(title = 'Case study 0',
             subtitle = 'From the data concatenated without tapering',
             caption = 'orange lines show the 95% confidence limit of zero GC at each frequency') +
        theme(panel.background = element_rect(fill = NA, color = 'black'),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.background = element_rect(colour = 'black', fill = 'white'),
              legend.position = 'top') +
        facet_grid(cols = vars(cols), rows = vars(rows), scales = 'free_y')
    ggsave(output_filename, p, width = 6, height = 20)
}

#' @import ggplot2
#' @importFrom dplyr filter
#' @importFrom tibble rowid_to_column
#' @importFrom tidyr unnest
.study0_fig7 <- function(args, gf) {
    if (length(args) < 2)
        stop("missing arguments")

    pdf_file <- args[1]
    rugc_file <- args[2]

    load(rugc_file)

    data1 <- tbl_rugc %>%
        mutate(cols = sprintf('F1 = %g\nF2 = %g\nF3 = %g', Fcoef1, Fcoef2, Fcoef3),
               rows = sprintf('Q = %g', Qcoef))
    data2 <- data1 %>%
        unnest(ugc)

    p <- ggplot(data1) +
        geom_vline(aes(xintercept = Fcoef1), colour = 'red', linewidth = 0.3) +
        geom_text(aes(x = Fcoef1, label = 'F1'), y = Inf, vjust = 'inward', colour = 'red') +
        geom_vline(aes(xintercept = Fcoef2), colour = 'green', linewidth = 0.3) +
        geom_text(aes(x = Fcoef2, label = 'F2'), y = Inf, vjust = 'inward', colour = 'green') +
        geom_vline(aes(xintercept = Fcoef3), colour = 'blue', linewidth = 0.3) +
        geom_text(aes(x = Fcoef3, label = 'F3'), y = Inf, vjust = 'inward', colour = 'blue') +
        geom_vline(aes(xintercept = Fcoef1 + Fcoef2), colour = 'pink', linewidth = 0.3) +
        geom_text(aes(x = Fcoef1 + Fcoef2), y = Inf, vjust = 'inward', label = 'F1 + F2', colour = 'pink', size = 2) +
        geom_vline(aes(xintercept = 100 - (Fcoef1 + Fcoef2)), colour = 'pink', linewidth = 0.3) +
        geom_text(aes(x = 100 - (Fcoef1 + Fcoef2)), y = Inf, vjust = 'inward', label = '100 - (F1 + F2)', colour = 'pink', size = 2) +
        gf(aes(x = frequency, y = value), data = data2) +
        geom_hline(aes(yintercept = limit), colour = 'orange', linewidth = 0.3, alpha = 0.8) +
        geom_text(aes(label = sprintf('threshold: %s', format.pval(limit))), x = Inf, y = 0, vjust = 'inward', hjust ='inward', colour = 'orange', alpha = 0.8) +
        geom_text(aes(label = sprintf('p = %d', p)), x = Inf, y = Inf, vjust = 'inward', hjust = 'inward', colour = 'black', alpha = 0.8) +
        coord_cartesian(xlim = c(0, .sampling_rate/2)) +
        xlab('Frequency [Hz]') +
        ylab('Unconditional GC of X3 from X1 * X2') +
        labs(title = 'Case study 0',
             subtitle = 'From the data concatenated without tapering',
             caption = 'orange lines show the pFDR 95% confidence limit of zero GC at each frequency') +
        theme(panel.background = element_rect(fill = NA, color = 'black'),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.background = element_rect(colour = 'black', fill = 'white'),
              legend.position = 'top') +
        facet_grid(cols = vars(cols), rows = vars(rows), scales = 'free_y')
    ggsave(pdf_file, p, width = 6, height = 20)
}

## commands

#' @importFrom tibble tribble
#' @importFrom utils write.csv
#' @export
study0_conf <- function() {
    x <- tribble(
        ~Fcoef1, ~Fcoef2, ~Fcoef3,
        80/(2*pi), 70/(2*pi), 120/(2*pi),
        80/(2*pi), 70/(2*pi), 50-10/(2*pi),
        80/(2*pi), 70/(2*pi), 230/(2*pi),
        120/(2*pi), 70/(2*pi), 80/(2*pi),
        120/(2*pi), 70/(2*pi), 50-10/(2*pi),
        120/(2*pi), 70/(2*pi), 230/(2*pi),
        120/(2*pi), 70/(2*pi), 150/(2*pi),
        150/(2*pi), 70/(2*pi), 120/(2*pi),
        150/(2*pi), 70/(2*pi), 80/(2*pi),
        150/(2*pi), 70/(2*pi), 50-10/(2*pi),
        150/(2*pi), 70/(2*pi), 230/(2*pi),
        230/(2*pi), 70/(2*pi), 120/(2*pi),
        230/(2*pi), 70/(2*pi), 80/(2*pi),
        230/(2*pi), 70/(2*pi), 50-10/(2*pi),
        230/(2*pi), 70/(2*pi), 150/(2*pi),
        50-10/(2*pi), 70/(2*pi), 120/(2*pi),
        50-10/(2*pi), 70/(2*pi), 80/(2*pi),
        50-10/(2*pi), 70/(2*pi), 230/(2*pi),
        50-10/(2*pi), 70/(2*pi), 150/(2*pi),
        120/(2*pi), 80/(2*pi), 70/(2*pi),
        120/(2*pi), 80/(2*pi), 50-10/(2*pi),
        120/(2*pi), 80/(2*pi), 230/(2*pi),
        120/(2*pi), 80/(2*pi), 150/(2*pi),
        150/(2*pi), 80/(2*pi), 120/(2*pi),
        150/(2*pi), 80/(2*pi), 70/(2*pi),
        150/(2*pi), 80/(2*pi), 50-10/(2*pi),
        150/(2*pi), 80/(2*pi), 230/(2*pi),
        230/(2*pi), 80/(2*pi), 120/(2*pi),
        230/(2*pi), 80/(2*pi), 70/(2*pi),
        230/(2*pi), 80/(2*pi), 50-10/(2*pi),
        230/(2*pi), 80/(2*pi), 150/(2*pi),
        50-10/(2*pi), 80/(2*pi), 120/(2*pi),
        50-10/(2*pi), 80/(2*pi), 70/(2*pi),
        50-10/(2*pi), 80/(2*pi), 230/(2*pi),
        50-10/(2*pi), 80/(2*pi), 150/(2*pi),
        150/(2*pi), 120/(2*pi), 70/(2*pi),
        150/(2*pi), 120/(2*pi), 80/(2*pi),
        150/(2*pi), 120/(2*pi), 50-10/(2*pi),
        150/(2*pi), 120/(2*pi), 230/(2*pi),
        230/(2*pi), 120/(2*pi), 70/(2*pi),
        230/(2*pi), 120/(2*pi), 80/(2*pi),
        230/(2*pi), 120/(2*pi), 50-10/(2*pi),
        230/(2*pi), 120/(2*pi), 150/(2*pi),
        50-10/(2*pi), 120/(2*pi), 70/(2*pi),
        50-10/(2*pi), 120/(2*pi), 80/(2*pi),
        50-10/(2*pi), 120/(2*pi), 230/(2*pi),
        50-10/(2*pi), 120/(2*pi), 150/(2*pi),
        230/(2*pi), 150/(2*pi), 120/(2*pi),
        230/(2*pi), 150/(2*pi), 70/(2*pi),
        230/(2*pi), 150/(2*pi), 80/(2*pi),
        230/(2*pi), 150/(2*pi), 50-10/(2*pi),
        50-10/(2*pi), 150/(2*pi), 120/(2*pi),
        50-10/(2*pi), 150/(2*pi), 70/(2*pi),
        50-10/(2*pi), 150/(2*pi), 80/(2*pi),
        50-10/(2*pi), 150/(2*pi), 230/(2*pi),
        50-10/(2*pi), 230/(2*pi), 120/(2*pi),
        50-10/(2*pi), 230/(2*pi), 70/(2*pi),
        50-10/(2*pi), 230/(2*pi), 80/(2*pi),
        50-10/(2*pi), 230/(2*pi), 150/(2*pi))
    y <- data.frame(Qcoef = c(0.025, 0.050, 0.075, 0.150, 0.300, 0.750))
    z <- merge(x, y, all = TRUE)
    data <- cbind(name = sapply(seq_len(nrow(z)), function(i) {
        sprintf("conf%03d", i)
    }), z)

    for (i in seq_len(nrow(data))) {
        dir.create(data$name[i], recursive = TRUE, showWarnings = FALSE)
        write.csv(z[i,],
                  file = file.path(data$name[i], 'parameters.txt'),
                  quote = FALSE,
                  row.names = FALSE)
    }

    write.csv(data, file = 'conf.csv', row.names = FALSE)
}

#' @importFrom stats rnorm runif
#' @importFrom utils read.csv
#' @export
study0_sim <- function(args) {
    if (length(args) < 5)
        stop("missing arguments")

    output_filename <- args[1]
    input_filename <- args[2]

    x <- as.integer(args[3])
    y <- as.integer(args[4])
    z <- as.integer(args[5])

    param <- read.csv(input_filename)

    i1 <- function(x, p) {sin(2 * pi * param$Fcoef1 * x + p)}
    i2 <- function(x, p) {sin(2 * pi * param$Fcoef2 * x + p)}
    i3 <- function(x, p) {cos(2 * pi * param$Fcoef3 * x + p)}

    tc <- function(k) {
        set.seed(k)
        ps <- runif(3, min = 0, max = 2*pi)
        function(x) {
            xi1 <- rnorm(length(x), mean = 0, sd = 1)
            xi2 <- rnorm(length(x), mean = 0, sd = 1)
            xi3 <- rnorm(length(x), mean = 0, sd = 1)
            s1 <- i1(x, ps[1])
            s2 <- i2(x, ps[2])
            s3 <- param$Qcoef * s1 * s2 +
                i3(x, ps[3])
            c1 <- s1 + xi1
            c2 <- s2 + xi2
            c3 <- param$Qcoef * c1 * c2 +
                i3(x, ps[3]) + xi3
            data.frame(xi1, xi2, xi3, s1, s2, s3, c1, c2, c3)
        }
    }

    sample_tc <- function() {
        Map(function(f) {f(seq_len(.study0_simulation_length) / .sampling_rate)}, Map(tc, seq_len(.study0_sample_size)))
    }

    xi3_data_frame <- function(y) {
        do.call(cbind, Map(function(k) {y[[k]]$xi3}, seq_len(.study0_sample_size)))
    }

    s3_data_frame <- function(y) {
        do.call(cbind, Map(function(k) {y[[k]]$s3}, seq_len(.study0_sample_size)))
    }

    c1_data_frame <- function(y) {
        do.call(cbind, Map(function(k) {y[[k]]$c1}, seq_len(.study0_sample_size)))
    }

    c2_data_frame <- function(y) {
        do.call(cbind, Map(function(k) {y[[k]]$c2}, seq_len(.study0_sample_size)))
    }

    c3_data_frame <- function(y) {
        do.call(cbind, Map(function(k) {y[[k]]$c3}, seq_len(.study0_sample_size)))
    }

    y1 <- sample_tc()
    data <- list(xi3_data_frame(y1),
                 s3_data_frame(y1),
                 c1_data_frame(y1),
                 c2_data_frame(y1),
                 c3_data_frame(y1))

    long_data <- do.call(rbind, y1)

    save(data, long_data, file = output_filename)
}

#' @importFrom readr write_csv
#' @export
study0_csv <- function(args) {
    if (length(args) < 2)
        stop('missing arguments')

    output_filename <- args[1]
    input_filename <- args[2]

    load(input_filename)
    readr::write_csv(long_data, output_filename)
}

#' @importFrom multitaper spec.mtm
#' @export
study0_psd <- function(args) {
    if (length(args) < 2)
        stop('missing arguments')

    output_filename <- args[1]
    input_filename <- args[2]

    load(input_filename)

    psd1 <- spec.mtm(ts(long_data$c1, frequency = .sampling_rate), plot = FALSE)
    psd2 <- spec.mtm(ts(long_data$c2, frequency = .sampling_rate), plot = FALSE)
    psd3 <- spec.mtm(ts(long_data$c3, frequency = .sampling_rate), plot = FALSE)

    save(psd1, psd2, psd3,
         file = output_filename)
}

#' @importFrom dplyr bind_rows filter
#' @export
study0_fig_psd <- function(args) {
    if (length(args) < 2)
        stop('missing arguments')

    output_filename <- args[1]
    conf_name <- args[2]

    conf <- read.csv("conf.csv") %>%
        filter(name == conf_name)

    stopifnot(nrow(conf) == 1)

    F1 <- conf$Fcoef1
    F2 <- conf$Fcoef2
    F3 <- conf$Fcoef3
    Q <- conf$Qcoef

    .data_frame <- function(psd, i, type) {
        data.frame(freq = psd$freq,
                   spec = psd$spec,
                   i = sprintf('X_%d', i),
                   type = type)
    }

    load(file.path(conf_name, 'psd-01-02-03.RData'))

    data1 <- .data_frame(psd1, 1, '-')
    data2 <- .data_frame(psd2, 2, '-')
    data3 <- .data_frame(psd3, 3, '-')

    data <- bind_rows(data1, data2, data3)

    p <- ggplot() +
        geom_vline(xintercept = F1, colour = 'red', linewidth = 0.3) +
        geom_text(x = F1, y = 0.95, label = 'F1', colour = 'red') +
        geom_vline(xintercept = F2, colour = 'green', linewidth = 0.3) +
        geom_text(x = F2, y = 0.95, label = 'F2', colour = 'green') +
        geom_vline(xintercept = F3, colour = 'blue', linewidth = 0.3) +
        geom_text(x = F3, y = 0.95, label = 'F3', colour = 'blue') +
        geom_vline(xintercept = F1 + F2, colour = 'pink', linewidth = 0.3) +
        geom_text(x = F1 + F2, y = 0.99, label = 'F1 + F2', colour = 'pink', size = 2) +
        geom_vline(xintercept = F1 - F2, colour = 'pink', linewidth = 0.3) +
        geom_text(x = F1 - F2, y = 0.99, label = 'F1 - F2', colour = 'pink', size = 2) +
        geom_vline(xintercept = 100 - (F1 + F2), colour = 'pink', linewidth = 0.3) +
        geom_text(x = .sampling_rate - (F1 + F2), y = 0.99, label = '100 - (F1 + F2)', colour = 'pink', size = 2) +
        geom_line(aes(freq, spec), data = data, linewidth = 0.5) +
        scale_y_log10() +
        coord_cartesian(xlim = c(0, .sampling_rate/2)) +
        xlab('Frequency [Hz]') +
        ylab('Estimated power spectral density') +
        labs(title = 'Case study 0',
             subtitle = sprintf('F1 = %g, F2 = %g, F3 = %g, Q = %g', F1, F2, F3, Q)) +
        theme(panel.background = element_rect(fill = NA, color = 'black'),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.background = element_rect(colour = 'black', fill = 'white'),
              legend.position = 'top') +
        facet_grid(rows = vars(type), cols = vars(i))
    ggsave(output_filename, p, width = 20, height = 8)
}

#' @export
study0_bqpc <- function(args) {
    if (length(args) < 3)
        stop('missing arguments')

    output_filename <- args[1]
    param_filename <- args[2]
    data_filename <- args[3]

    param <- read.csv(param_filename)
    load(data_filename)

    x <- long_data$c1
    y <- long_data$c2
    z <- long_data$c3
    bqpc <- .bqpc(x, y, z,
                  nw = .study0_nw, ntp_in_window = .study0_ntp_in_window,
                  bin_width = .bin_width,
                  sampling_rate = .sampling_rate,
                  F1 = param$Fcoef1, F2 = param$Fcoef2)
    save(bqpc, file = output_filename)
}

#' @export
study0_gqpc <- function(args) {
    if (length(args) < 3)
        stop('missing arguments')

    output_filename <- args[1]
    param_filename <- args[2]
    data_filename <- args[3]

    param <- read.csv(param_filename)
    load(data_filename)

    x <- long_data$c1
    y <- long_data$c2
    z <- long_data$c3
    gqpc <- .gqpc(x, y, z,
                  nw = .study0_nw, ntp_in_window = .study0_ntp_in_window,
                  sampling_rate = .sampling_rate,
                  F1 = param$Fcoef1, F2 = param$Fcoef2)
    save(gqpc, file = output_filename)
}

#' @importFrom stats dchisq
#' @export
study0_xbcmap <- function(args) {
    dachisq <- function(x, df, ncp, rate) {
        rate * stats::dchisq(rate * x, df, ncp = ncp)
    }

    .filename_to_F_NQ <- function(filename) {
        conf_name <- strsplit(filename, "/")[[1]][1]
        param_file <- sprintf("%s/parameters.txt", conf_name)
        params <- read.csv(param_file)
        ## F <- sprintf("(f1, f2, f3) = (%g, %g, %g)", params$Fcoef1, params$Fcoef2, params$Fcoef3)
        F <- sprintf("F1 = %g", params$Fcoef1)
        NQ <- sprintf("Q = %g", params$Qcoef)
        list(Fcoef1 = params$Fcoef1,
             Fcoef2 = params$Fcoef2,
             F = F, NQ = NQ)
    }

    if (length(args) < 2)
        stop("missing arguments")

    output_filename <- args[1]
    rest <- args[-1]

    out <- do.call(rbind, Map(function(filename, i) {
        lst <- .filename_to_F_NQ(filename)
        load(filename)
        cbind(bqpc$xbc_Hz, Fcoef1 = lst$Fcoef1, Fcoef2 = lst$Fcoef2, F = lst$F, NQ = lst$NQ)
    }, rest, seq_len(length(rest))))

    stats <- do.call(rbind, Map(function(filename, i) {
        lst <- .filename_to_F_NQ(filename)
        load(filename)
        m1 <- mean(bqpc$xbc_Hz$value)
        m2 <- median(bqpc$xbc_Hz$value)
        v <- var(bqpc$xbc_Hz$value)
        label <- sprintf("sample mean: %g\nsample median: %g\nsample variance: %g\nt_max: %g\np0: %g\n", m1, m2, v, bqpc$mm$t_max, exp(bqpc$mm$log_p0))
        data.frame(x = 1,
                   y = Inf,
                   label = label,
                   F = lst$F, NQ = lst$NQ)
    }, rest, seq_len(length(rest))))

    fit <- do.call(rbind, Map(function(filename, i) {
        lst <- .filename_to_F_NQ(filename)
        load(filename)
        x <- seq(0, 1, by = 0.001)
        z <- data.frame(x = x,
                        y = dachisq(x, df = bqpc$mm$nu, ncp = 0, rate = 1/bqpc$mm$a))
        cbind(z, F = lst$F, NQ = lst$NQ)
    }, rest, seq_len(length(rest))))

    save(out, stats, fit, file = output_filename)
}

#' @importFrom cowplot plot_grid
#' @importFrom dplyr distinct if_else select
#' @importFrom magrittr %>%
#' @export
study0_fig_xbcmap <- function(args) {
    .bd1 <- function(out, stats, fit) {
        ggplot(out, aes(value)) +
            geom_histogram(aes(y = after_stat(density)), binwidth = 0.001) +
            geom_line(aes(x = x, y = y), data = fit, colour = "blue") +
            geom_text(aes(x = x, y = y, label = label), data = stats, vjust = "inward", hjust = "inward") +
            xlab("raw estimated value of cross-bicoherence") +
            labs(title = 'Case study 0',
                 subtitle = 'From the concatenated data without tapering') +
            theme(panel.background = element_rect(fill = NA, color = "black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.background = element_rect(colour = "black", fill = "white")) +
            facet_wrap(vars(F, NQ), nrow = 2)
    }

    .xbc <- function(out_Hz, xout) {
        ggplot() +
            geom_tile(aes(f1_Hz, f2_Hz, fill = value, colour = value), data = out_Hz) +
            geom_vline(aes(xintercept = Fcoef1), data = xout, colour = "green", alpha = 0.5, linewidth = 0.1) +
            geom_hline(aes(yintercept = if_else(Fcoef1 + Fcoef2 < .sampling_rate/2, Fcoef2, -Fcoef2)), data = xout, colour = "green", alpha = 0.5, linewidth = 0.1) +
            scale_fill_gradient(limits = c(0, 1), low = "gray90", high = "darkblue") +
            scale_colour_gradient(limits = c(0, 1), low = "gray90", high = "darkblue") +
            coord_fixed(xlim = c(0, .sampling_rate/2), ylim = c(-.sampling_rate/2, .sampling_rate/2)) +
            xlab(expression(f[1] ~ "[Hz]")) +
            ylab(expression(f[2] ~ "[Hz]")) +
            theme(panel.background = element_rect(fill = NA, color = "black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.background = element_rect(colour = "black", fill = "white"),
                  plot.margin = unit(c(0, 0.3, 0, 0), "cm"),
                  legend.position = "top") +
            facet_wrap(vars(F, NQ), nrow = 2)
    }

    .map <- function(out_Hz, xout) {
        ggplot() +
            geom_tile(aes(f1_Hz, f2_Hz, fill = significance), data = out_Hz) +
            geom_vline(aes(xintercept = Fcoef1), data = xout, colour = "green", alpha = 0.5, linewidth = 0.1) +
            geom_hline(aes(yintercept = if_else(Fcoef1 + Fcoef2 < .sampling_rate/2, Fcoef2, -Fcoef2)), data = xout, colour = "green", alpha = 0.5, linewidth = 0.1) +
            scale_fill_manual(values = c("Y" = "red", 'N' = NA), na.value = 'white') +
            coord_fixed(xlim = c(0, .sampling_rate/2), ylim = c(-.sampling_rate/2, .sampling_rate/2)) +
            xlab(expression(f[1] ~ "[Hz]")) +
            ylab(expression(f[2] ~ "[Hz]")) +
            theme(panel.background = element_rect(fill = NA, color = "black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.background = element_rect(colour = "black", fill = "white"),
                  plot.margin = unit(c(0, 0.3, 0, 0), "cm"),
                  legend.position = "top") +
            facet_wrap(vars(F, NQ), nrow = 2)
    }

    if (length(args) < 2)
        stop("missing arguments")

    output_filename <- args[1]
    xbcmap_filename <- args[2]

    load(xbcmap_filename)

    out$significance <- as.factor(out$significance)

    p1 <- .bd1(out, stats, fit)
    out_Hz <- within(out, {
        f1_Hz <- f1 * .sampling_rate
        f2_Hz <- f2 * .sampling_rate
    })
    xout <- out %>%
        select(Fcoef1, Fcoef2, F, NQ) %>%
        distinct()
    p2 <- .xbc(out_Hz, xout)
    p3 <- .map(out_Hz, xout)

    ## p <- plot_grid(NULL, p1, p2, p3,
    ##                labels = "AUTO",
    ##                align = "hv",
    ##                ncol = 2)
    g1 <- plot_grid(p1, labels = "A")
    g2 <- plot_grid(p2, p3,
                    labels = c("B", "C"),
                    align = "hv",
                    axis = 'tblr')
    g <- plot_grid(g1, g2,
                   align = "h",
                   ncol = 1,
                   rel_heights = c(1, 2))
    ggsave(output_filename, g, width = 36, height = 15)
}

#' @export
study0_fig_mvgc_line <- function(args) {
    .plot_mvgc(args[1], geom_line)
}

#' @export
study0_fig_mvgc_area <- function(args) {
    .plot_mvgc(args[1], geom_area)
}

#' @export
study0_ugctest_ic <- function(args) {
    if (length(args) < 3)
        stop("missing arguments")

    output_filename <- args[1]
    input_filename <- args[2]
    ic <- args[3]

    load(input_filename)

    x1 <- long_data$c1
    x2 <- long_data$c2
    x3 <- long_data$c3
    x12 <- x1 * x2
    ugctest <- .Granger.inference.unconditional(x3, x12, type.chosen = "none", ic.chosen = ic, max.lag = 50, nboots = 100)
    save(ugctest, file = output_filename)
}

#' @importFrom psych describe
#' @importFrom readr read_csv
#' @export
study0_tbl_snr <- function(args) {
    if (length(args) < 1)
        stop("missing arguments")

    output_file <- args[1]

    conf <- read_csv("conf.csv", show_col_types = FALSE)
    snr <- map_dfr(seq_len(nrow(conf)), function(i) {
        load(file.path(conf$name[i], 'data-01-02-03.RData'))
        d <- describe(long_data$c3)
        data.frame(Fcoef1 = conf$Fcoef1[i],
                   Fcoef2 = conf$Fcoef2[i],
                   Fcoef3 = conf$Fcoef3[i],
                   Qcoef = conf$Qcoef[i],
                   snr1 = .snr(long_data$s1, long_data$xi1),
                   snr2 = .snr(long_data$s2, long_data$xi2),
                   snr08 = .snr(long_data$c3 - long_data$xi3, long_data$xi3),
                   snr10 = .snr(long_data$s3, long_data$xi3),
                   snr13 = .snr(long_data$s3, long_data$c3 - long_data$s3),
                   ind1 = d$sd^2,
                   ind2 = sqrt(d$mad),
                   ind3 = sqrt(d$range))
    })
    save(snr, file = output_file)
}

#' @importFrom readr read_csv
#' @export
study0_tbl_rxbc <- function(args) {
    if (length(args) < 1)
        stop("missing arguments")

    output_filename <- args[1]

    conf <- read_csv("conf.csv", show_col_types = FALSE)
    tbl_rxbc <- map_dfr(seq_len(nrow(conf)), function(i) {
        load(file.path(conf$name[i], 'bqpc-01-02-03.RData'))
        data.frame(Fcoef1 = conf$Fcoef1[i],
                   Fcoef2 = conf$Fcoef2[i],
                   Fcoef3 = conf$Fcoef3[i],
                   Qcoef = conf$Qcoef[i],
                   R_BQPC = bqpc$R_BQPC)
    })
    save(tbl_rxbc, file = output_filename)
}

#' @importFrom dplyr if_else nest_by
#' @importFrom purrr map_dfr
#' @importFrom readr read_csv
#' @importFrom tibble tibble
#' @export
study0_tbl_rugc <- function(args) {
    if (length(args) < 1)
        stop("missing arguments")

    output_file <- args[1]

    conf <- read_csv('conf.csv', show_col_types = FALSE)
    tbl_rugc <- map_dfr(seq_len(nrow(conf)), function(i) {
        load(file.path(conf$name[i], 'gqpc-01-02-03.RData'))
        tibble(Fcoef1 = conf$Fcoef1[i],
               Fcoef2 = conf$Fcoef2[i],
               Fcoef3 = conf$Fcoef3[i],
               Qcoef = conf$Qcoef[i],
               p = gqpc$p,
               R_GQPC = gqpc$R_GQPC,
               limit = gqpc$median_limit,
               ugc = list(gqpc$median_ugc))
    })
    save(tbl_rugc, file = output_file)
}

#' @importFrom dplyr left_join mutate select
#' @export
study0_tbl_ratio <- function(args) {
    if (length(args) < 4)
        stop("missing arguments")

    output_filename <- args[1]
    load(args[2])
    load(args[3])
    load(args[4])

    F123Q <-  c('Fcoef1','Fcoef2', 'Fcoef3', 'Qcoef')

    tbl_ratio <- snr %>%
        left_join(tbl_rxbc, by = F123Q) %>%
        left_join(tbl_rugc, by = F123Q) %>%
        select(Fcoef1, Fcoef2, Fcoef3, Qcoef, snr08, snr10, snr13, R_BQPC, R_GQPC) %>%
        mutate(F = sprintf('(F1, F2, F3) = (%g, %g, %g)', Fcoef1, Fcoef2, Fcoef3))

    save(tbl_ratio, file = output_filename)
}

#' @import ggplot2
#' @importFrom dplyr mutate
#' @export
study0_fig_snr <- function(args) {
    if (length(args) < 2)
        stop("missing arguments")

    output_filename <- args[1]
    input_filename <- args[2]

    load(input_filename)

    data1 <- snr %>%
        mutate(F = sprintf('(F1, F2, F3) = (%g, %g, %g)', Fcoef1, Fcoef2, Fcoef3))

    p <- ggplot(data1) +
        geom_function(fun = .snrA, colour = 'black', linetype = 'dotted') +
        geom_function(fun = .snrB, colour = 'black', linetype = 'dashed') +
        geom_function(fun = .snrC, colour = 'black', linetype = 'solid') +
        geom_point(aes(Qcoef, snr08, group = F, colour = F), alpha = 0.5, shape = 'plus') +
        geom_point(aes(Qcoef, snr10, group = F, colour = F), alpha = 0.5, shape = 'cross') +
        geom_point(aes(Qcoef, snr13, group = F, colour = F), alpha = 0.5, shape = 'asterisk') +
        geom_point(aes(Qcoef, ind1, group = F, colour = F), alpha = 0.5, shape = 'circle open') +
        geom_point(aes(Qcoef, ind2, group = F, colour = F), alpha = 0.5, shape = 'triangle open') +
        geom_point(aes(Qcoef, ind3, group = F, colour = F), alpha = 0.5, shape = 'square open') +
        ylim(0, NA) +
        xlab('Q') +
        ylab('SNR indicator') +
        labs(title = 'Case study 0',
             subtitle = 'From the data concatenated without tapering',
             caption = paste0('Pluses: SNR(Q * X_1 * X_2 + I_3, xi3) calculated from the concatenated data;\n',
                              'Crosses: SNR(Q * I_1 * I_2 + I_3, xi3) calculated from the concatenated data;\n',
                              'Asterisks: SNR(Q * I_1 * I_2 + I_3, X3 - (Q * I_1 * I_2 + I_3)) calculated from the concatenated data\n',
                              '(with its analytical solution of solid line);\n',
                              'Circles: the variance; Triangles: the square root of the MAD; Squares: the square root of the range\n',
                              'Black lines: the analytical solution of SNR(Q * X_1 * X_2 + I_3, xi3) (dotted) or SNR(Q * I_1 * I_2 + I_3, xi3) (dashed)')) +
        theme(panel.background = element_rect(fill = NA, color = 'black'),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.background = element_rect(colour = 'black', fill = 'white'),
              legend.position = 'top')
    ggsave(output_filename, p, width = 7, height = 6)
}

#' @import ggplot2
#' @importFrom dplyr mutate
#' @export
study0_fig_ratio <- function(args) {
    if (length(args) < 2)
        stop("missing arguments")

    output_filename <- args[1]
    input_filename <- args[2]

    load(input_filename)

    data <- tbl_ratio %>%
        mutate(F = sprintf('(F1, F2, F3) = (%g, %g, %g)', Fcoef1, Fcoef2, Fcoef3))

    p <- ggplot(data) +
        geom_point(aes(Qcoef, R_GQPC, group = F, colour = F)) +
        geom_line(aes(Qcoef, R_GQPC, group = F, colour = F), linetype = 'dashed') +
        geom_point(aes(Qcoef, R_BQPC, group = F, colour = F,)) +
        geom_line(aes(Qcoef, R_BQPC, group = F, colour = F), linetype = 'solid') +
        ylim(0, 1) +
        ylab('Ratio') +
        labs(title = 'Case study 0',
             subtitle = 'From the data concatenated without tapering') +
        theme_classic() +
        theme(legend.position = 'top')
    ggsave(output_filename, p, width = 7, height = 7)
}

#' @import ggplot2
#' @importFrom dplyr mutate transmute
#' @importFrom tidyr unnest
#' @export
study0_fig6 <- function(args) {
    if (length(args) < 2)
        stop("missing arguments")

    pdf_file <- args[1]
    rugc_file <- args[2]

    load(rugc_file)

    data1 <- tbl_rugc %>%
        mutate(cols = sprintf('F1 = %g\nF2 = %g\nF3 = %g', Fcoef1, Fcoef2, Fcoef3),
               rows = sprintf('Q = %g', Qcoef))
    data2 <- data1 %>%
        unnest(ugc)

    p <- ggplot(data1) +
        geom_vline(aes(xintercept = Fcoef1), colour = 'red', linewidth = 0.3) +
        geom_text(aes(x = Fcoef1, y = 0.95, label = 'F1'), colour = 'red') +
        geom_vline(aes(xintercept = Fcoef2), colour = 'green', linewidth = 0.3) +
        geom_text(aes(x = Fcoef2, y = 0.95, label = 'F2'), colour = 'green') +
        geom_vline(aes(xintercept = Fcoef3), colour = 'blue', linewidth = 0.3) +
        geom_text(aes(x = Fcoef3, y = 0.95, label = 'F3'), colour = 'blue') +
        geom_vline(aes(xintercept = Fcoef1 + Fcoef2), colour = 'pink', linewidth = 0.3) +
        geom_text(aes(x = Fcoef1 + Fcoef2), y = 0.99, label = 'F1 + F2', colour = 'pink', size = 2) +
        geom_vline(aes(xintercept = 100 - (Fcoef1 + Fcoef2)), colour = 'pink', linewidth = 0.3) +
        geom_text(aes(x = 100 - (Fcoef1 + Fcoef2)), y = 0.99, label = '100 - (F1 + F2)', colour = 'pink', size = 2) +
        geom_line(aes(x = frequency, y = value), data = data2, linewidth = 0.5) +
        geom_hline(aes(yintercept = limit), colour = 'orange', linewidth = 0.3, alpha = 0.8) +
        geom_text(aes(label = sprintf('threshold: %s', format.pval(limit))), x = Inf, y = 0, vjust = 'inward', hjust ='inward', colour = 'orange', alpha = 0.8) +
        geom_text(aes(label = sprintf('p = %d', p)), x = Inf, y = Inf, vjust = 'inward', hjust = 'inward', colour = 'black', alpha = 0.8) +
        scale_colour_manual(values = c('TRUE' = 'grey10', 'FALSE' = 'grey90')) +
        scale_fill_manual(values = c('TRUE' = 'grey10', 'FALSE' = 'grey90')) +
        coord_cartesian(xlim = c(0, .sampling_rate/2), ylim = c(0, 1)) +
        xlab('Frequency [Hz]') +
        ylab('Unconditional GC of X3 from X1 * X2') +
        labs(title = 'Case study 0',
             subtitle = 'From the data concatenated without tapering',
             caption = 'orange lines show the pFDR 95% confidence limit of zero GC at each frequency') +
        theme(panel.background = element_rect(fill = NA, color = 'black'),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.background = element_rect(colour = 'black', fill = 'white'),
              legend.position = 'top') +
        facet_grid(cols = vars(cols), rows = vars(rows))
    ggsave(pdf_file, p, width = 6, height = 20)
}

#' @export
study0_fig7_line <- function(args) {
    .study0_fig7(args, geom_line)
}

#' @export
study0_fig7_area <- function(args) {
    .study0_fig7(args, geom_area)
}
