# ToDo:
#   doreport --handled by Email Module
#   ifsreduce
#   exam.ksh
#   bin2csv
#   copy-csv
#   extract-csv
#   dotransfer
.PHONY : links
SCRIPTS=IFScheck IFSloop OpusHdr bitcvt.sh
LINKS=OpusHdr IFSloop IFScheck
TGTDIR=/home/citco2/bin/anal
LINKDIR=/usr/local/bin

distribution :
	@[ -d $(TGTDIR) ] || mkdir $(TGTDIR) || \
	  echo Could not create $(TGTDIR)
	@distribute $(TGTDIR) $(SCRIPTS)
links: distribution $(LINKS)
	@for script in $(LINKS); do ln -sf $(TGTDIR)/$$script $(LINKDIR)/$$script; done 

