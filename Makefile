docs: man
	for i in $$(ls man/*.Rd); do\
		R CMD Rdconv -t html -o docs/$${i#man/}.html $$i;\
	done
