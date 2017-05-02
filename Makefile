TDIR=../DramaAnalysis.wiki

wiki: ${TDIR}/Configuration-Matrices.md ${TDIR}/Figure-Statistics.md 

vignettes/%.md: vignettes/%.Rmd
	Rscript -e "library(rmarkdown); render('$<', output_format='md_document', clean=TRUE)"
	
${TDIR}/%.md: vignettes/%.md
	rm -rf ${TDIR}/$*_files
	mv -f vignettes/$*_files ${TDIR}/
	mv -f $< ${TDIR}/


all: 

clean: 
	rm -r *_files

push: all
	git add .
	git commit -a -m "Run"
	git push