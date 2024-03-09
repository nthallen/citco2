# IFS.spec
tmcbase = base.tmc
Module tmbase mode=ignore
Module savelog
Module Email dest=/home/citco2/bin/Email

TGTDIR = /home/citco2
SCRIPT = VERSION Experiment.config
IGNORE = Makefile *.exe
