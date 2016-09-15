# DramaAnalysis with R

This package contains a number of functions that work with the QuaDramA web service.


## Building

Generate HTML doc in `docs` directory

```sh
for i in $(ls man/*.Rd); do R CMD Rdconv -t html -o docs/${i#man/}.html $i; done
```

