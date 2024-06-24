# -*- Mode: makefile-gmake; tab-width: 4; indent-tabs-mode: t -*-

RSCRIPT_STUDY1 = Rscript --vanilla R/study1.R

## definitions

conf_names := $(foreach i,$(shell seq 1 354),$(shell printf 'conf%03d' $(i)))

define per_conf
$(1)/bqpc-01-02-03.RData: $(1)/parameters.txt $(1)/data-01-02-03.RData
	$(RSCRIPT_STUDY1) bqpc $$@ $$^

$(1)/gqpc-01-02-03.RData: $(1)/parameters.txt $(1)/data-01-02-03.RData
	$(RSCRIPT_STUDY1) gqpc $$@ $$^
endef

## recipes

.PHONY: clean

$(foreach cn,$(conf_names),$(eval $(call per_conf,$(cn))))

study1-xbcmap-01-02-03.RData: $(foreach cn,$(conf_names),$(cn)/bqpc-01-02-03.RData)
	$(RSCRIPT_STUDY1) xbcmap $@ $^

study1-fig-xbcmap-01-02-03.pdf: study1-xbcmap-01-02-03.RData
	$(RSCRIPT_STUDY1) fig-xbcmap $@ $<

study1-fig6.pdf: study1-tbl-rugc.RData
	$(RSCRIPT_STUDY1) fig6 $@ $<

study1-fig7-line.pdf: study1-tbl-rugc.RData
	$(RSCRIPT_STUDY1) fig7-line $@ $<

study1-fig7-area.pdf: study1-tbl-rugc.RData
	$(RSCRIPT_STUDY1) fig7-area $@ $<

study1-tbl-snr.RData: $(foreach cn,$(conf_names),$(cn)/data-01-02-03.RData)
	$(RSCRIPT_STUDY1) tbl-snr $@

study1-tbl-snr.csv: study1-tbl-snr.RData
	Rscript --vanilla -e "library(readr); load('$<'); write_csv(snr, '$@')"

study1-tbl-rxbc.RData: $(foreach cn,$(conf_names),$(cn)/bqpc-01-02-03.RData)
	$(RSCRIPT_STUDY1) tbl-rxbc $@

study1-tbl-rugc.RData: $(foreach cn,$(conf_names),$(cn)/gqpc-01-02-03.RData)
	$(RSCRIPT_STUDY1) tbl-rugc $@

study1-tbl-ratio.RData: study1-tbl-snr.RData study1-tbl-rxbc.RData study1-tbl-rugc.RData
	$(RSCRIPT_STUDY1) tbl-ratio $@ $^

study1-fig-snr.pdf: study1-tbl-snr.RData
	$(RSCRIPT_STUDY1) fig-snr $@ $<

study1-fig-ratio.pdf: study1-tbl-ratio.RData
	$(RSCRIPT_STUDY1) fig-ratio $@ $<

BQPC = \
	$(foreach cn,$(conf_names),$(cn)/bqpc-01-02-03.RData) \
	study1-xbcmap-01-02-03.RData \
	study1-fig-xbcmap-01-02-03.pdf \
	study1-tbl-rxbc.RData

study1-bqpc.tar.gz: $(BQPC)
	tar -czf $@ $^

GQPC = \
	$(foreach cn,$(conf_names),$(cn)/gqpc-01-02-03.RData) \
	study1-tbl-rugc.RData \
	study1-fig7-line.pdf \
	study1-fig7-area.pdf

study1-gqpc.tar.gz: $(GQPC)
	tar -czf $@ $^

RESULTS = \
	study1-fig-ratio.pdf \
	study1-tbl-ratio.RData

study1.tar.gz: $(RESULTS)
	tar -czf $@ $^

clean:
	-rm *.tar.gz *.pdf *.RData $(foreach cn,$(conf_names),$(cn)/*.pdf $(cn)/*.csv $(cn)/*.RData)

.DEFAULT_GOAL := study1.tar.gz
