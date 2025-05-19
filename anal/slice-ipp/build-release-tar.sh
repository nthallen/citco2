#!/bin/sh

# This script builds the distribution tar files for "slice-ipp" and
# "opus-ipp" (and maybe other programs later).
#
# The source files are located in a CVS sandbox which must be the
# first argument to the script.  The second argument is the program
# name (e.g., slice-ipp).  The distribution file name will include
# the program version number, derived by parsing the Fortran source
# file.  And there is a third optional argument to this script to
# specify the version number of the distribution tar file.  This
# "extra-version" parameter allows for fixes in configuration files
# that would not update the program version number.
#
# The intent is to automate the release and decrease the risk of
# human errors.  The script starts with an extensive error check
# of its input parameters.  It also performs some checks about
# the local environment (e.g., the version of 'tar').
#
# It makes sure there is a default Makefile and then uses the
# 'mrproper' target to remove all non-source files.
#
# A GNU-style ChangeLog is built automatically from the CVS logs
# by the 'cvs2cl.pl' script which can be found at:
# 
#   http://www.red-bean.com/cvs2cl/
#   http://www.ing.iac.es/~docs/external/gnu/cvs2cl.html
#
# The only requirement is that 'cvs2cl.pl' must be on the $PATH
# You may be prompted for your smirnov password so that the CVS
# logs can be extracted.
#
# Finally, the distribution tar file is created in two steps:
# 1) The files are assembled in a directory under $HOME.
# 2) This directory is archived in the distribution tar file,
#    also placed in $HOME.
#
# The assembly directory is not deleted so that you can inspect
# its contents.

if [ $# != 2 ] && [ $# != 3 ]; then
  echo "Usage: build-release-tar SANDBOX PROGRAM [EXTRAVERSION]"
  exit 1
fi

if [ $# = 3 ]; then
  EXTVER="-"$3
else
  EXTVER=""
fi

SAND=$1
PROG=$2
ADIR=$SAND/anal
PDIR=$ADIR/$PROG

if [ ! -d "$SAND" ]; then
  echo "Cannot find sandbox ""$SAND"
  exit 1
fi

if [ ! -d "$PDIR" ]; then
  echo "Cannot find program directory ""$PDIR"
  exit 1
fi

if [ ! -f "$PDIR"/"$PROG".f ]; then
  echo "Cannot find program source file ""$PDIR"/"$PROG".f
  exit 1
fi

if [ ! -f "$PDIR"/"$PROG"-version.inc ]; then
  echo "Cannot find program version file ""$PDIR"/"$PROG"-version.inc
  exit 1
fi

CVS2CL=`which cvs2cl.pl`
# This works around an atrocious bug in the /usr/bin/which of Solaris:
# CVS2CL=`echo $CVS2CL | cat -vt | sed -e "s/^\^\[\]2;//" -e "s/\^G.//"`
if [ ! -x "$CVS2CL" ]; then
  echo "Install script cvs2cl.pl so that ChangeLog can be built"
  exit 1
fi
#DIAG echo $CVS2CL

if tar --version 2>/dev/null | grep GNU >/dev/null ; then
  TARP=GNU
  #DIAG echo "Found GNU tar"
else
  TARP=UNIX
  #DIAG echo "Assuming UNIX tar"
fi

SRCH="& '"$PROG
#DIAG echo $SRCH
VER=`grep "$SRCH" "$PDIR"/"$PROG"-version.inc | sed "s/  */ /g" | cut -d " " -f 5`
VERNAM=$PROG-$VER$EXTVER
#DIAG echo $VERNAM

cd "$PDIR" || exit
echo "Cleaning sandbox directory: ""$PDIR"
if [ ! -f Makefile ]; then
  ln -s Makefile.gfortran Makefile
fi
make mrproper

echo "Creating ChangeLog"
cd ..
$CVS2CL --separate-header --no-wrap --stdout ./*-ipp \
  | tr '\t' "~" | sed "s/^~/   /" > "$PROG"/ChangeLog
cd "$PROG" || exit

REL=$HOME/$VERNAM
echo "Building release directory: ""$REL"
mkdir "$REL"

# KLUDGE: temporarily duplicate slice-ipp files for opus-ipp
if [ "$PROG" = opus-ipp ]; then
  rm -fr comn opus-comn ipp-lite.f
  cp -pr ../slice-ipp/comn .
  cp -pr ../slice-ipp/opus-comn/ .
  cp -p ../slice-ipp/ipp-lite.f .
fi
if [ "$PROG" = slice-ipp ]; then
  cd ../.. || exit
  # find config -name flimit.ipp -print | cpio -pdvm $REL
  find config/full_analysis -name flimit.ipp -print | cpio -pdvm "$REL"
  # find config -name $PROG.top -print | cpio -pdvm $REL
  find config/full_analysis -name "$PROG".top -print | cpio -pdvm "$REL"
  cd anal/"$PROG" || exit
fi
find . -name CVS -type d -print > "$HOME"/xclude
find . -name .cvsignore -type f -print >> "$HOME"/xclude
find . -name build-release-tar.sh -type f -print >> "$HOME"/xclude
if [ $TARP = GNU ]; then
  tar -cf - -X "$HOME"/xclude . | (cd "$REL" || exit; tar -xvpf -)
else
  tar -cfX - "$HOME"/xclude . | (cd "$REL" || exit; tar -xvpf -)
fi
rm "$HOME"/xclude

# UNKLUDGE: re-establish symbolic links to slice-ipp files for opus-ipp
if [ "$PROG" = opus-ipp ]; then
  rm -fr comn opus-comn ipp-lite.f
  ln -s ../slice-ipp/comn comn
  ln -s ../slice-ipp/opus-comn opus-comn
  ln -s ../slice-ipp/ipp-lite.f ipp-lite.f
fi

echo "Building release archive: ""$REL".tar
cd || exit
tar -cvf "$VERNAM".tar "$VERNAM"

