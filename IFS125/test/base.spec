prefix = base
Module TMbase mode=ignore
TGTDIR = /home/IFSbase
SCRIPT = VERSION Experiment.config
DISTRIB = interact services
DISTRIB = TestClient
DISTRIB = ../mClient.pm
DISTRIB = ../IFS_msg.pm
DISTRIB = ../ColSend.pm
DISTRIB = ../IFSq.pm
IGNORE = Makefile '*.exe'

tmcbase = base.tmc
cmdbase = IFSscan.cmd
cmdbase = ../TM/IFS.cmd

basedisp : IFSbase.tbl
basealgo : IFSbase.tma
doit : base.doit
