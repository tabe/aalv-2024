suppressPackageStartupMessages(library(qpcstudies))

.main <- function() {
    args <- commandArgs(trailingOnly = TRUE)
    if (length(args) < 1)
        stop('missing arguments')

    rest <- args[-1]

    switch(args[1],
           'conf' = study0_conf(),
           'sim' = study0_sim(rest),
           'csv' = study0_csv(rest),
           'psd' = study0_psd(rest),
           'fig-psd' = study0_fig_psd(rest),
           'bqpc' = study0_bqpc(rest),
           'gqpc' = study0_gqpc(rest),
           'xbcmap' = study0_xbcmap(rest),
           'fig-xbcmap' = study0_fig_xbcmap(rest),
           'fig-mvgc-line' = study0_fig_mvgc_line(rest),
           'fig-mvgc-area' = study0_fig_mvgc_area(rest),
           'tbl-snr' = study0_tbl_snr(rest),
           'tbl-rxbc' = study0_tbl_rxbc(rest),
           'tbl-rugc' = study0_tbl_rugc(rest),
           'tbl-ratio' = study0_tbl_ratio(rest),
           'fig-snr' = study0_fig_snr(rest),
           'fig-ratio' = study0_fig_ratio(rest),
           'fig6' = study0_fig6(rest),
           'fig7-line' = study0_fig7_line(rest),
           'fig7-area' = study0_fig7_area(rest),
           stop('invalid first argument'))
}

.main()
