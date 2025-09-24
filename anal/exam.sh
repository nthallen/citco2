#!/bin/bash

function nl_error {
  echo "exam.sh: $*" >&2
  exit 1
}

# Set parameters
   echo The input of exam.sh is $*  #YZH
   unset noclobber
   # The analysis should take place under /d1/home/citco2
   # on container systems, but this is handled vi soft
   # links in /home/citco2, allowing this to work on
   # development systems as well.
   HOME=/home/citco2
   ANAL=$HOME/bin/anal
   IPP=$HOME/bin/ipp
   DP=$HOME/dp

# Get the name of the slice directories
   Parentdir=''
   Slicedirs=''
   for dir in $*; do
     myParent=$(dirname $dir)
     [ -n "$Parentdir" ] || Parentdir=$myParent
     if [ ! "$Parentdir" -ef "$myParent" ]; then
       echo Run $dir cannot be processed with runs from $Parentdir
     else
       Slicedirs="$Slicedirs $dir"
     fi
   done
 
# For DP: Diagnostic Procedure
   DPBIN=$DP/bin
   SUNRUN=$DP/sunruns/gnd
   RUNLOG=$DP/runlogs/gnd
   if [ -d /removable/citco2 ]; then
     SPEC=/removable/citco2/spectra
     SPECTMP=/removable/citco2/spectra.tmp
   else
     SPEC=/home/citco2/spectra
     SPECTMP=/home/citco2/spectra.tmp
   fi

ipp_srcs=$(ls $IPP/slice-ipp*.top)
for ipp_src in $ipp_srcs; do
  scantype=${ipp_src##*slice-ipp}
  scantype=${scantype%.top}
  scanarg=''
  [ -n "$scantype" ] && scanarg="-scantype=${scantype#.}"
  [ -f $IPP/slice-ipp$scantype.top ] ||
    nl_error "Missing configuration file: $IPP/slice-ipp$scantype.top"
  [ -f $IPP/flimit$scantype.ipp ] ||
    nl_error "Missing configuration file: $IPP/flimit$scantype.ipp"
  if [ -n "$scantype" ]; then
    echo "\nProcessing ${scantype#.} scans\n"
  else
    echo "\nProcessing all scans\n"
  fi

# Run slice-ipp to form spectra
   ( cat <<EOF
: This is the input file for the 'slice-ipp' program.
: This version was assembled automatically by the
: script $0 including content from
: $IPP/slice-ipp$scantype.top and the catalog script.

$Parentdir/
$SPECTMP/
EOF
     cat $IPP/slice-ipp$scantype.top
     $ANAL/catalog $scanarg $Slicedirs
   ) >slice-ipp.in
   cp -f $IPP/flimit$scantype.ipp flimit.ipp
   $IPP/slice-ipp
   rm -f slice-ipp.in flimit.ipp
done

if [ -d $DPBIN ]; then
# Generate the name of the sunrun and runlog
   N=(date +%Y%m%d)
   M=$(basename $(ls $SPECTMP/??20??????s*a.* | head -1) | cut -c 1-2)
   N=$M$N
   WORK=$DP/work/$N
   RUNNAME=$N.gnd

# Check if the working directory already
# exists.
   if [ -d $WORK ]; then
     echo $WORK exists 
   else
     echo $WORK will be created
     mkdir $WORK
   fi

# Go to the spectral directory and form the list
# of spectra
   echo "Writing sunrun & Moving spectra"
   ( cd $SPECTMP
     ls ??20??????s*a.* > $SUNRUN/$RUNNAME
       # I move them out of the tmp directory now so:
       # a) the fitting software will find them in $SPEC
       # b) they are already cleared out of this directory
       #    in case we have to stop the fitting early.
     mv * $SPEC
   )

#
   echo "Begin spectra analysis" 
   ( cd $DPBIN; echo $RUNNAME  | ./runlog; )
   ( cd $WORK; echo $RUNNAME | $DPBIN/gsetup;
     chmod +x multiggg.bat; ./multiggg.bat
     $DPBIN/dp_report
   )
   echo "0 [$WORK/$N.dp_report] Fitting Summary" >>/data/citco2/report.queue
else
  echo "Skipping spectra analysis"
  # echo "0 Overnight Analysis Completed" >>/data/citco2/report.queue
fi


   set noclobber

