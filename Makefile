TDIR=../DramaAnalysis.wiki
VIG=vignettes
VERSION=$(shell grep -o -e 'Version:.*' DESCRIPTION | egrep -o '\d+\.\d+\.\d+')

check:
	cd .. && R CMD build DramaAnalysis && R CMD check DramaAnalysis_${VERSION}.tar.gz

wiki: ${TDIR}/Configuration-Matrices.md ${TDIR}/Figure-Statistics.md ${TDIR}/Word-Field-Analysis.md ${TDIR}/Presence.md ${TDIR}/Loading-Texts.md

${VIG}/%.md: vignettes/%.Rmd ${VIG}/version.md ${VIG}/vig-%.md
	Rscript -e "library(rmarkdown); render('$<', output_format='md_document', clean=TRUE)"


${TDIR}/%.md: vignettes/%.md
	rm -rf ${TDIR}/$*_files
	mv -f vignettes/$*_files ${TDIR}/ || true
	mv -f $< ${TDIR}/ || true

${VIG}/version.md: DESCRIPTION
	grep -o -e 'Version:.*' DESCRIPTION  | egrep -o '\d+\.\d+\.\d+' | xargs perl -e 'my $$v = shift; print "![$$v](https://img.shields.io/badge/v-$$v-blue.svg)";' > ${VIG}/version.md

${VIG}/vig-%.md: vignettes/%.Rmd
	echo "This guide is also available as a vignette in the R console: \`vignette($*)\`.\n\n---\n" > $@

all: 

clean: 
	rm -r *_files

push: all
	git add .
	git commit -a -m "Run"
	git push
