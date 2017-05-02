TDIR=../DramaAnalysis.wiki
VIG=vignettes

wiki: ${TDIR}/Configuration-Matrices.md ${TDIR}/Figure-Statistics.md 

${VIG}/%.md: vignettes/%.Rmd ${VIG}/version.md
	Rscript -e "library(rmarkdown); render('$<', output_format='md_document', clean=TRUE)"
	
${TDIR}/%.md: vignettes/%.md
	rm -rf ${TDIR}/$*_files
	mv -f vignettes/$*_files ${TDIR}/
	mv -f $< ${TDIR}/

${VIG}/version.md: DESCRIPTION
	grep -o -e 'Version:.*' DESCRIPTION  | egrep -o '\d+\.\d+\.\d+' | xargs perl -e 'my $$v = shift; print "![$$v](https://img.shields.io/badge/v-$$v-blue.svg)";' > ${VIG}/version.md

all: 

clean: 
	rm -r *_files

push: all
	git add .
	git commit -a -m "Run"
	git push