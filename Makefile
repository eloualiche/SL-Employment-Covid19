#
# (c) Daniel Green and Erik Loualiche
# 
# Replication code for _State and Local Government Employment in the COVID-19 Crisis_
# Journal of Public Economics 
#
# This file builds the datasets and generate all tables in the paper
# See the readme for dependencies and architecture of the code
#
# --------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
all: unzip_data \
  build_census_finance build_lau build_census_employment \
  build_cps_full build_reg \
  build_main_figs_tables build_appendix_tables \
  build_paper


# --- Unzip the data 
unzip_data:
	unzip_data:
	tar xf data.zip
	mkdir -p code/log
	mkdir -p derived
	mkdir -p output
	mkdir -p output/appendix
	mkdir -p output/figures
	mkdir -p output/tables
	$(TIME-END)
	@echo 

# --- build the CoG Revenue/Expenditure finance files
build_census_finance:
	mkdir -p code/log
	cd code; R CMD BATCH --vanilla assemble_1.R log/assemble_1.log.R
	$(TIME-END)
	@echo

# --- local area employment statistics
build_lau: 	
	cd code; R CMD BATCH --vanilla assemble_2.R log/assemble_2.log.R
	$(TIME-END)
	@echo

# --- build the CoG employment side
build_census_employment:
	cd code; R CMD BATCH --vanilla assemble_3.R log/assemble_3.log.R
	$(TIME-END)
	@echo

# --- read the CPS 
build_cps_full:
	cd code; $(STATA_MP_CLI) -b do assemble_4.do
	mv code/assemble_4.log code/log/assemble_4.log.do 
	$(TIME-END)
	@echo

# --- create the regression datasets before we generate tables
build_reg:
	cd code; R CMD BATCH --vanilla assemble_5.R log/assemble_5.log.R
	$(TIME-END)
	@echo

# --- generate summary statistics table	
build_main_figs_tables: 
	cd code; R CMD BATCH --vanilla create_table_1.R log/create_table_1.log.R
	cd code; $(STATA_MP_CLI) -b do create_figs_tables.do
	mv code/create_figs_tables.log code/log/create_figs_tables.log.do 
	$(TIME-END)
	@echo

# --- generate Appendix tables
build_appendix_tables:
	cd code; R CMD BATCH --vanilla create_appendix_table_1.R log/create_appendix_table_1.log.R
	cd code; R CMD BATCH --vanilla create_appendix_table_2.R log/create_appendix_table_2.log.R
	cd code; R CMD BATCH --vanilla create_appendix_table_3.R log/create_appendix_table_3.log.R
	cd code; R CMD BATCH --vanilla create_appendix_table_8.R log/create_appendix_table_8.log.R
	cd code; $(STATA_MP_CLI) -b do create_appendix_figs_tables.do
	mv code/create_appendix_figs_tables.log code/log/create_appendix_figs_tables.log.do 
	$(TIME-END)
	@echo

# - build paper
build_paper:
	cd manuscript; pdflatex -interaction=batchmode -output-directory ./ localgov_GL_jpube.tex
	cd manuscript; pdflatex -interaction=batchmode -output-directory ./ localgov_GL_jpube.tex
	rm -f manuscript/*.aux manuscript/*.log manuscript/*.lot manuscript/*.out manuscript/*.toc manuscript/*.bbl manuscript/*.blg
	mv manuscript/localgov_GL_jpube.pdf ./
	$(TIME-END)
	@echo
# ------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
# - generate figures for website
build_website_figures:
	pdf2png -s @2x -i output/figures/cares_logpop_rf_no_controls.pdf -o output/figures/cares_logpop_rf_no_controls
	pdf2png -s @8x -i output/figures/cares_logpop_rf_no_controls.pdf -o output/figures/cares_logpop_rf_no_controls
	mv output/figures/cares_logpop_rf_no_controls@8x.png output/figures/cares_logpop_rf_no_controls.png


# - clean 
clean.all: 
	rm -f output/tables/*.tex
	rm -f output/figures/*.pdf
	rm -f output/appendix/tables/*.tex
	rm -f output/appendix/figures/*.pdf
	rm -f code/log/**
	rm -f derived/**
	rm -rf data
# ------------------------------------------------------------------------------------------


# ----------------------------------------------------------------------------
# CHANGE THIS TO WHERE YOUR STATA BINARY IS
STATA_MP_CLI := /Applications/Stata/StataMP.app/Contents/MacOS/stata-mp	

TIME_START := $(shell date +%s)
WHITE='\033[1;37m'
NC   ='\033[0m' # No Color
define TIME-END
 	@time_end=`date +%s` ; time_exec=`awk -v "TS=${TIME_START}" -v "TE=$$time_end" 'BEGIN{TD=TE-TS;printf "%02dm:%02ds\n", TD/(60),TD%60}'` ; echo \\t${WHITE}cumulative time elapsed ... $${time_exec} ... $@ ${NC}
endef
# ------------------------------------------------------------------------------------------
