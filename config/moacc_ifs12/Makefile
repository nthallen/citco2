# use 'make FORCE=-f' to force an update.
.PHONY : distribution config-phocus config-gse config-TM config-report config-slice-ipp

config-TM :
	[ -e Site.agm ] || ln -sf Site.agm.phocus Site.agm
	rm -f ../../TM/Config
	ln -sf ../config/moacc_ifs12 ../../TM/Config
config-phocus : config-TM
	ln -sf Site.agm.phocus Site.agm
config-gse : config-TM
	ln -sf Site.agm.gse Site.agm
config-report :
	distribute $(FORCE) /home/citco2/bin/Email doreport.pm
config-slice-ipp :
	rm -f /home/citco2/ipp/slice-ipp*.top /home/citco2/ipp/flimit*.ipp
	distribute $(FORCE) /home/citco2/ipp slice-ipp*.top flimit*.ipp

#distribution : config-TM config-report config-slice-ipp
distribution : config-TM config-report
