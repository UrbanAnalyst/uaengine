.PHONY: all build check document test

RFILE = README

all: document build check

build: doc
	R CMD build .

clean:
	-rm -f utaengine*tar.gz
	-rm -fr utaengine.Rcheck
	#-rm -fr src/*.{o,so}

doc: clean
	Rscript -e 'devtools::document()'
	Rscript -e 'rmarkdown::render("$(RFILE).Rmd",rmarkdown::md_document(variant="gfm"))'

knith:
	Rscript -e 'rmarkdown::render("$(RFILE).Rmd",output_file="$(RFILE).html")'

test:
	Rscript -e 'devtools::test()'

check:
	Rscript -e 'library(pkgcheck); checks <- pkgcheck(); print(checks); summary (checks)'

install: clean
	R CMD INSTALL .
