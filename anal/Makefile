# ToDo:
#   doreport --handled by Email Module
#   ifsreduce
#   exam.ksh
#   OpusHdr
#   bin2csv
#   copy-csv
#   extract-csv
#   removable --probably obsolete
#   dotransfer
SCRIPTS=IFScheck IFSloop
TGTDIR=/home/citco2/bin/anal
distribution :
	@[ -d $(TGTDIR) ] || mkdir $(TGTDIR) || \
	  echo Could not create $(TGTDIR)
	@distribute $(TGTDIR) $(SCRIPTS)
