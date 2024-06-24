# -*- Mode: makefile-gmake; tab-width: 4; indent-tabs-mode: t -*-

RSCRIPT_STUDY0 = Rscript --vanilla R/study0.R

## definitions

conf_names := $(foreach i,$(shell seq 1 354),$(shell printf 'conf%03d' $(i)))

define per_conf
$(1)/parameters.txt: conf.csv

$(1)/data-01-02-03.RData: $(1)/parameters.txt
	$(RSCRIPT_STUDY0) sim $$@ $$< 1 2 3

$(1)/data-01-02-03.csv: $(1)/data-01-02-03.RData
	$(RSCRIPT_STUDY0) csv $$@ $$<

$(1)/psd-01-02-03.RData: $(1)/data-01-02-03.RData
	$(RSCRIPT_STUDY0) psd $$@ $$<

$(1)/psd-01-02-03.pdf: $(1)/psd-01-02-03.RData
	$(RSCRIPT_STUDY0) fig-psd $$@ $(1)
endef

## recipes

.PHONY: clean

conf.csv:
	$(RSCRIPT_STUDY0) conf

$(foreach cn,$(conf_names),$(eval $(call per_conf,$(cn))))

study0-tbl-snr.RData: $(foreach cn,$(conf_names),$(cn)/data-01-02-03.RData)
	$(RSCRIPT_STUDY0) tbl-snr $@

study0-tbl-snr.csv: study0-tbl-snr.RData
	Rscript --vanilla -e "library(readr); load('$<'); write_csv(snr, '$@')"

study0-fig-snr.pdf: study0-tbl-snr.RData
	$(RSCRIPT_STUDY0) fig-snr $@ $<

DATA = \
	conf.csv \
	$(foreach cn,$(conf_names),$(cn)/parameters.txt) \
	$(foreach cn,$(conf_names),$(cn)/data-01-02-03.RData) \
	$(foreach cn,$(conf_names),$(cn)/data-01-02-03.csv) \
	$(foreach cn,$(conf_names),$(cn)/psd-01-02-03.RData) \
	$(foreach cn,$(conf_names),$(cn)/psd-01-02-03.pdf) \
	study0-tbl-snr.RData \
	study0-tbl-snr.csv \
	study0-fig-snr.pdf

study0-data.tar.gz: $(DATA)
	tar -czf $@ $^

.DEFAULT_GOAL := study0-data.tar.gz
