.PHONY: all build check doc test

RFILE = README
VFILE = benchmarks

all: doc build check

build: doc
	R CMD build .

clean:
	-rm -f utaengine*tar.gz
	-rm -fr utaengine.Rcheck
	#-rm -fr src/*.{o,so}

doc: clean
	Rscript -e 'devtools::document()'
	Rscript -e 'rmarkdown::render("$(RFILE).Rmd",rmarkdown::md_document(variant="gfm"))'
	Rscript -e "pkgdown::init_site()"
	Rscript -e "pkgdown::build_article('$(VFILE)',quiet=FALSE)"

knith:
	Rscript -e 'rmarkdown::render("$(RFILE).Rmd",output_file="$(RFILE).html")'

readme:
	Rscript -e 'rmarkdown::render("$(RFILE).Rmd",rmarkdown::md_document(variant="gfm"))'

test:
	Rscript -e 'devtools::test()'

check:
	Rscript -e 'library(pkgcheck); checks <- pkgcheck(); print(checks); summary (checks)'

install: clean
	R CMD INSTALL .
