# Makefile for R package DramaAnalysis

docs: man
	for i in $$(ls man/*.Rd); do\
		R CMD Rdconv -t html -o docs/$${i#man/}.html $$i;\
	done

index:
	rm -f docs/index.html
	echo "<ul>" >> docs/index.html
	for i in $$( ls docs/*.html ); do\
		echo "<li><a href=\"$${i#docs/}\">$${i%.Rd.html}</a></li>" >> docs/index.html;\
	done
	echo "</ul>" >> docs/index.html
