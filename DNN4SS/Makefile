PKG_NAME=$(shell grep -i ^package DESCRIPTION | cut -d : -d \  -f 2)

# Install package
install:
	cd .. && R CMD INSTALL $(PKG_NAME)

# Instal dependencies
#depend:
#	Rscript -e 'install.packages(c("MASS", "abc", "synlik", "R6", "SimInf", "parallel", "Matrix", "data.table", "akima", "plot3D", "lattice", "pbapply", "GGally", "ggthemes", "grid", "ggplot2"), repos="http://cran.rstudio.com")'
# Build documentation with roxygen
# 1) Remove old doc
# 2) Generate documentation
roxygen:
	rm -f man/*.Rd
	Rscript -e "library(devtools)" \
                -e "devtools::document()"

test:
	R CMD check
