PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename $(PWD))
TGZ     := ../$(PKGSRC)_$(PKGVERS).tar.gz
TGZVNR  := ../$(PKGSRC)_$(PKGVERS)-vignettes-not-rebuilt.tar.gz

# Specify the directory holding R binaries. To use an alternate R build (say a
# pre-prelease version) use `make RBIN=/path/to/other/R/` or `export RBIN=...`
# If no alternate bin folder is specified, the default is to use the folder
# containing the first instance of R on the PATH.
RBIN ?= $(shell dirname "`which R`")

# Specify directories for subversion repository on r-forge
RFSVN ?= $(HOME)/svn/gwidgets
RFDIR ?= $(RFSVN)/pkg/gWidgetsWWW2

pkgfiles = DESCRIPTION \
	   NAMESPACE \
           NEWS \
	   README.md \
	   TODO.md \
	   DESCRIPTION \
	   demo/* \
	   inst/apps/* \
	   inst/base/css/gWidgetsWWW2.css \
	   inst/base/images/* \
	   inst/base/javascript/* \
	   inst/base/javascript/CodeMirror/* \
	   inst/base/javascript/ext-4-2.1/* \
	   inst/base/javascript/ext-4-2.1/locale/* \
	   inst/base/javascript/ext-4-2.1/resources/css/ext-all.css \
	   inst/base/javascript/ext-4-2.1/resources/ext-theme-classic/* \
	   inst/base/javascript/ext-4-2.1/resources/ext-theme-classic/images/*/* \
	   inst/base/javascript/ext-4-2.1/resources/themes/images/default/*/* \
	   inst/ex_data/states.json \
	   inst/examples/* \
	   inst/framework/brew/* \
	   inst/framework/templates/* \
	   inst/tests/* \
	   inst/CITATION \
	   man/* \
	   R/* \
           vignettes/gWidgetsWWW2.pdf

$(TGZ): $(pkgfiles)
	cd ..;\
		"$(RBIN)/R" CMD build $(PKGSRC)

$(TGZVNR): $(pkgfiles)
	cd ..;\
		"$(RBIN)/R" CMD build $(PKGSRC) --no-build-vignettes;\
		cd $(PKGSRC);\
	mv $(TGZ) $(TGZVNR)
                
build: $(TGZ)

build-no-vignettes: $(TGZVNR)

install: build
	"$(RBIN)/R" CMD INSTALL $(TGZ)

install-no-vignettes: build-no-vignettes
	"$(RBIN)/R" CMD INSTALL $(TGZVNR)

check: build
	# Vignettes have been rebuilt by the build target
	"$(RBIN)/R" CMD check --as-cran --no-build-vignettes $(TGZ)

check-no-vignettes: build-no-vignettes
	mv $(TGZVNR) $(TGZ)
	"$(RBIN)/R" CMD check --as-cran $(TGZ)
	mv $(TGZ) $(TGZVNR)

vignettes/gWidgetsWWW2.pdf: vignettes/gWidgetsWWW2.Rnw
	"$(RBIN)/Rscript" -e "tools::buildVignette(file = 'vignettes/gWidgetsWWW2.Rnw', dir = 'vignettes')"

vignettes: vignettes/gWidgetsWWW2.pdf

r-forge:
	git archive master > $(HOME)/gWidgetsWWW2.tar;\
	cd $(RFDIR) && rm -r `ls` && tar -xf $(HOME)/gWidgetsWWW2.tar;\
	svn add --force .; svn rm --force `svn status | grep "\!" | cut -d " " -f 8`; cd $(RFSVN) && svn commit -m 'sync with https://github.com/jverzani/gWidgetsWWW2'

clean: 
	$(RM) -r $(PKGNAME).Rcheck/
	$(RM) vignettes/*.R
