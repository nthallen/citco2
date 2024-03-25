# IFS.spec
TGTDIR = /home/citco2
SCRIPT = VERSION Experiment.config
IGNORE = Makefile *.exe

tmcbase = base.tmc
Module tmbase mode=ignore
Module savelog
Module Email dest=/home/citco2/bin/Email
Module PTB330

IFSdisp : $extbase IFS.tbl
