suppressPackageStartupMessages(library(qpcstudies))

.main <- function() {
    args <- commandArgs(trailingOnly = TRUE)
    if (length(args) < 1)
        stop('missing arguments')

    rest <- args[-1]

    switch(args[1],
           'conf' = study1_conf(),
           'sim' = study1_sim(rest),
           'csv' = study1_csv(rest),
           'psd' = study1_psd(rest),
           'fig-psd' = study1_fig_psd(rest),
           'bqpc' = study1_bqpc(rest),
           'gqpc' = study1_gqpc(rest),
           'xbcmap' = study1_xbcmap(rest),
           'fig-xbcmap' = study1_fig_xbcmap(rest),
           'fig-mvgc-line' = study1_fig_mvgc_line(rest),
           'fig-mvgc-area' = study1_fig_mvgc_area(rest),
           'tbl-snr' = study1_tbl_snr(rest),
           'tbl-rxbc' = study1_tbl_rxbc(rest),
           'tbl-rugc' = study1_tbl_rugc(rest),
           'tbl-ratio' = study1_tbl_ratio(rest),
           'fig-snr' = study1_fig_snr(rest),
           'fig-ratio' = study1_fig_ratio(rest),
           'fig6' = study1_fig6(rest),
           'fig7-line' = study1_fig7_line(rest),
           'fig7-area' = study1_fig7_area(rest),
           stop('invalid first argument'))
}

.main()
