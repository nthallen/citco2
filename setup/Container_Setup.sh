#! /bin/sh

# Container_Setup.sh
# Script to do one-time setup tasks.
#
# This script is to be run after the operating system has been
# installed, the network configured, users defined or imported,
# sudo configured and monarch-citco2-install.sh has been run,
# but before the instrument code has been compiled.
# 

function nl_error {
  echo Container_Setup: $* >&2
  exit 1
}

if [ $(uname -s | sed -e 's/-.*$//') = CYGWIN_NT ]; then
  is_cygwin=yes
  flightuser=
else
  is_cygwin=no
  flightuser=flight
fi  

SPECTRADIR=/home/citco2/spectra.tmp
ANALDIR=/home/citco2/bin/anal
IPPDIR=/home/citco2/bin/ipp
EMAILDIR=/home/citco2/bin/Email

function mkbindir {
  dir=$1
  [ -d $dir ] || mkdir -p $dir
  [ -d $dir ] || nl_error "Unable to create directory '$dir'"
  chown $flightuser:flight $dir
  chmod g+w $dir
}

if [ $(id -u) = 0 -o $is_cygwin = yes ]; then
  mkbindir $SPECTRADIR
  mkbindir $ANALDIR
  mkbindir $IPPDIR
  mkbindir $EMAILDIR
  ln -sf $ANALDIR/OpusHdr /usr/local/bin/OpusHdr
  ln -sf $ANALDIR/IFScheck /usr/local/bin/IFScheck
  # ln -sf $(ANALDIR)/removable /usr/local/bin/removable
  ln -sf $IPPDIR/slice-ipp /usr/local/bin/slice-ipp
  ln -sf /home/citco2/IFSloop /usr/local/bin/IFSloop
else
  echo /usr/bin/sudo $0 $*
  /usr/bin/sudo $0 $*
  # Now as a regular user:
  cd /home/citco2/src/config
  # Now need to select a configuration:
  # cd /home/citco2/src/config/$cfg
  # make distribution
  #
  # Then build slice-ipp which is needed to build IFSloop:
  # cd /home/citco2/src/anal/slice-ipp
  # make -f Makefile.qnx6-f2c distribution
  #
  # Finally, build IFSloop:
  # cd /home/citco2/src/QNX6/TM
  # make
  # make distribution
  #
  # Overnight analysis and other scripts need to be distributed:
  # cd /home/citco2/src/QNX6/anal
  # make distribution
fi
