# IFS.spec
TGTDIR = /home/citco2
DISTRIB = interact services
SCRIPT = VERSION Experiment.config
IGNORE = Makefile '*.exe' Config
IGNORE = SWData.h SWData.tmc SWData_col.tmc SWData.cmd
OBJ = SWData.h SWData.tmc SWData_col.tmc SWData.cmd

tmcbase = base.tmc
tmcbase = SWStat.tmc
genuibase = IFS.genui
swsbase = Config/IFS.sws
Module TMbase mode=ignore
Module savelog
Module Email dest=/home/citco2/bin/Email
Module PTB330
Module WTX530
Module SunTracker
Module STEnc mode=present

IFSdisp : $extbase IFS.tbl
IFSalgo : IFS.tma
doit : IFS.doit

CPPFLAGS = -I Config
