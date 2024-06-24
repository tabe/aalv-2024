# -*- Mode: makefile-gmake; tab-width: 4; indent-tabs-mode: t -*-

RSCRIPT_STUDY0 = Rscript --vanilla R/study0.R

## definitions

conf_names := $(foreach i,$(shell seq 1 354),$(shell printf 'conf%03d' $(i)))

define per_conf
$(1)/bqpc-01-02-03.RData: $(1)/parameters.txt $(1)/data-01-02-03.RData
	$(RSCRIPT_STUDY0) bqpc $$@ $$^

$(1)/gqpc-01-02-03.RData: $(1)/parameters.txt $(1)/data-01-02-03.RData
	$(RSCRIPT_STUDY0) gqpc $$@ $$^
endef

## recipes

.PHONY: clean

$(foreach cn,$(conf_names),$(eval $(call per_conf,$(cn))))

study0-xbcmap-01-02-03.RData: $(foreach cn,$(conf_names),$(cn)/bqpc-01-02-03.RData)
	$(RSCRIPT_STUDY0) xbcmap $@ $^

study0-fig-xbcmap-01-02-03.pdf: study0-xbcmap-01-02-03.RData
	$(RSCRIPT_STUDY0) fig-xbcmap $@ $<

study0-fig6.pdf: study0-tbl-rugc.RData
	$(RSCRIPT_STUDY0) fig6 $@ $<

study0-fig7-line.pdf: study0-tbl-rugc.RData
	$(RSCRIPT_STUDY0) fig7-line $@ $<

study0-fig7-area.pdf: study0-tbl-rugc.RData
	$(RSCRIPT_STUDY0) fig7-area $@ $<

study0-tbl-snr.RData: $(foreach cn,$(conf_names),$(cn)/data-01-02-03.RData)
	$(RSCRIPT_STUDY0) tbl-snr $@

study0-tbl-snr.csv: study0-tbl-snr.RData
	Rscript --vanilla -e "library(readr); load('$<'); write_csv(snr, '$@')"

study0-tbl-rxbc.RData: $(foreach cn,$(conf_names),$(cn)/bqpc-01-02-03.RData)
	$(RSCRIPT_STUDY0) tbl-rxbc $@

study0-tbl-rugc.RData: $(foreach cn,$(conf_names),$(cn)/gqpc-01-02-03.RData)
	$(RSCRIPT_STUDY0) tbl-rugc $@

study0-tbl-ratio.RData: study0-tbl-snr.RData study0-tbl-rxbc.RData study0-tbl-rugc.RData
	$(RSCRIPT_STUDY0) tbl-ratio $@ $^

study0-fig-snr.pdf: study0-tbl-snr.RData
	$(RSCRIPT_STUDY0) fig-snr $@ $<

study0-fig-ratio.pdf: study0-tbl-ratio.RData
	$(RSCRIPT_STUDY0) fig-ratio $@ $<

BQPC = \
	$(foreach cn,$(conf_names),$(cn)/bqpc-01-02-03.RData) \
	study0-xbcmap-01-02-03.RData \
	study0-fig-xbcmap-01-02-03.pdf \
	study0-tbl-rxbc.RData

study0-bqpc.tar.gz: $(BQPC)
	tar -czf $@ $^

GQPC = \
	$(foreach cn,$(conf_names),$(cn)/gqpc-01-02-03.RData) \
	study0-tbl-rugc.RData \
	study0-fig7-line.pdf \
	study0-fig7-area.pdf

study0-gqpc.tar.gz: $(GQPC)
	tar -czf $@ $^

RESULTS = \
	study0-fig-ratio.pdf \
	study0-tbl-ratio.RData

study0.tar.gz: $(RESULTS)
	tar -czf $@ $^

clean:
	-rm *.tar.gz *.pdf *.RData $(foreach cn,$(conf_names),$(cn)/*.pdf $(cn)/*.csv $(cn)/*.RData)

.DEFAULT_GOAL := study0.tar.gz
