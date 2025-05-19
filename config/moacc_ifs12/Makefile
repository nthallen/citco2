# use 'make FORCE=-f' to force an update.
.PHONY : distribution config-phocus config-gse config-TM config-report config-slice-ipp

IPPDIR=/home/citco2/bin/ipp
EMAILDIR=/home/citco2/bin/Email

config-TM :
	[ -e Site.agm ] || ln -sf Site.agm.phocus Site.agm
	rm -f ../../TM/Config
	ln -sf ../config/moacc_ifs12 ../../TM/Config
config-phocus : config-TM
	ln -sf Site.agm.phocus Site.agm
config-gse : config-TM
	ln -sf Site.agm.gse Site.agm
config-report :
	distribute $(FORCE) $(EMAILDIR) doreport.pm
config-slice-ipp :
	rm -f $(IPPDIR)/slice-ipp*.top $(IPPDIR)/flimit*.ipp
	distribute $(FORCE) $(IPPDIR) slice-ipp*.top flimit*.ipp

distribution : config-TM config-report config-slice-ipp
