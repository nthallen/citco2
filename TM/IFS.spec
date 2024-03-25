# IFS.spec
TGTDIR = /home/citco2
DISTRIB = interact services
SCRIPT = VERSION Experiment.config
IGNORE = Makefile *.exe

tmcbase = base.tmc
Module TMbase mode=ignore
Module savelog
Module Email dest=/home/citco2/bin/Email
Module PTB330

IFSdisp : $extbase IFS.tbl
doit : IFS.doit
