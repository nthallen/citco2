prefix = base
Module TMbase mode=ignore
TGTDIR = /home/IFSbase
SCRIPT = VERSION Experiment.config
DISTRIB = interact services
IGNORE = Makefile '*.exe'

tmcbase = base.tmc
cmdbase = IFSscan.cmd
cmdbase = ../TM/IFS.cmd

basedisp : IFSbase.tbl
basealgo : IFSbase.tma
doit : base.doit
