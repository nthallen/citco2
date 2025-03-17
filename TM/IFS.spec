# IFS.spec
TGTDIR = /home/citco2
DISTRIB = interact services
SCRIPT = VERSION Experiment.config
IGNORE = Makefile '*.exe' Config
IGNORE = SWData.h SWData.tmc SWData_col.tmc SWData.cmd
OBJ = SWData.h SWData.tmc SWData_col.tmc SWData.cmd

tmcbase = base.tmc
extbase = WindSpeed.tmc
genuibase = IFS.genui
Module TMAstat TMA=Config/IFS.tma TextWidth=22
swsbase = Config/IFS.sws
cmdbase = Startup.cmd
Module TMbase mode=ignore SWSnot=
Module savelog
Module Email cfgsrcdir=Config/ dest=/home/citco2/bin/Email
Module PTB330
Module WTX530
Module SunTracker
Module MKS925
Module IFS125
Module LN2
Module webpower

Module Config/Site

IFSdisp : $extbase IFS.tbl IFS_HK.tbl
IFSalgo : $extbase Config/IFS.tma $swsbase
doit : IFS.doit

CPPFLAGS = -I Config -DBLOCK_KB_CMDS
