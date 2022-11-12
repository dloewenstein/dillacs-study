# Variables

RAW_DIR = ./analysis/data/raw_data
DATA_DIR = ./analysis/data/derived_data
FIG_DIR = ./analysis/figures
R_DIR = ./analysis/scripts
PAPER_DIR = ./analysis/paper
RFILES = $(filter-out $(R_DIR)/setup.R, $(wildcard $(R_DIR)/*.R))
ROUT = $(RFILES:.R=.Rout)
R = conda run --no-capture-output -n dillacs R CMD BATCH $< $(<:.R=.Rout)
KNIT = conda run --no-capture-output -n dillacs Rscript -e "require(bookdown); render_book('$<')"

purl=conda run --no-capture-output -n dillacs Rscript -e "knitr::purl('$(1)', '$(2)', quiet=TRUE, documentation=0)"

rfiles:=$(patsubst vignettes/LP-%.Rmd,analysis/scripts/%-GEN.R,$(wildcard vignettes/LP-*.Rmd))

all: data article-word article-pdf
article-word: $(PAPER_DIR)/_book/Loewenstein-et-al.---Diagnostic-performance-of-left-ventricular-mechanical-dyssynchrony-indices-using-CMR-feature-tracking.docx
article-pdf: $(PAPER_DIR)/_book/Loewenstein-et-al.---Diagnostic-performance-of-left-ventricular-mechanical-dyssynchrony-indices-using-CMR-feature-tracking.pdf
figs: $(R_DIR)/figures-GEN.Rout
data: scripts rout
rout: $(ROUT)
scripts: $(rfiles)

# special macros $@ and $^, which are the left and right sides of the :, respectively, to make the overall compilation rule more general.
$(PAPER_DIR)/_book/Loewenstein-et-al.---Diagnostic-performance-of-left-ventricular-mechanical-dyssynchrony-indices-using-CMR-feature-tracking.docx: $(wildcard $(PAPER_DIR)/*.Rmd) $(R_DIR)/statistics-GEN.Rout $(R_DIR)/figures-GEN.Rout | $(PAPER_DIR)
	cd $(PAPER_DIR) && \
	conda run --no-capture-output -n dillacs Rscript -e \
	"bookdown::render_book('abstract.Rmd', 'bookdown::word_document2')"

$(PAPER_DIR)/_book/Loewenstein-et-al.---Diagnostic-performance-of-left-ventricular-mechanical-dyssynchrony-indices-using-CMR-feature-tracking.pdf: $(wildcard $(PAPER_DIR)/*.Rmd) $(R_DIR)/statistics-GEN.Rout $(R_DIR)/figures-GEN.Rout | $(PAPER_DIR)
	cd $(PAPER_DIR) && \
	conda run --no-capture-output -n dillacs Rscript -e \
	"bookdown::render_book('abstract-pdf.Rmd', 'bookdown::pdf_book')"

$(R_DIR)/figures-GEN.Rout: $(R_DIR)/figures-GEN.R $(R_DIR)/statistics-GEN.Rout | $(FIG_DIR)
	$(R)

$(R_DIR)/statistics-GEN.Rout: $(R_DIR)/statistics-GEN.R $(R_DIR)/dyssynchrony_analysis-GEN.Rout
	$(R)

$(R_DIR)/dyssynchrony_analysis-GEN.Rout: $(R_DIR)/dyssynchrony_analysis-GEN.R $(R_DIR)/data_processing-GEN.Rout
	$(R)

$(R_DIR)/data_processing-GEN.Rout: $(R_DIR)/data_processing-GEN.R | $(DATA_DIR)
	$(R)

$(R_DIR)/%-GEN.R: vignettes/LP-%.Rmd | $(R_DIR)
	$(call purl,$^,$@)

$(R_DIR):
	mkdir -p $@

$(PAPER_DIR):
	mkdir -p $@

$(FIG_DIR):
	mkdir -p $@

$(DATA_DIR):
	mkdir -p $@

.PHONY: clean clean-figs data figs scripts all article rout

# Clean up genereated .outfiles and figures

clean:
	-rm -fv $(rfiles)
	-rm -fv $(wildcard $(R_DIR)/*.Rout)
	-rm -fv $(wildcard $(FIG_DIR)/*)

clean-figs:
	-rm -fv $(wildcard $(FIG_DIR)/*)
