# IFS.spec
TGTDIR = /home/citco2
DISTRIB = interact services
SCRIPT = VERSION Experiment.config
IGNORE = Makefile '*.exe'

tmcbase = base.tmc
genuibase = IFS.genui
Module TMbase mode=ignore
Module savelog
Module Email dest=/home/citco2/bin/Email
Module PTB330
Module WTX530

IFSdisp : $extbase IFS.tbl
IFSalgo : IFS.tma
doit : IFS.doit
