package IFS_msg;
use strict;

use Exporter 'import';
our VERSION = '1.00';
our @EXPORT_OK = qw(msg);

use Time::HiRes qw(gettimeofday);
use POSIX qw(floor);

use mClient;

my $memo;

sub msg {
  my $type = shift;
  $type = 2 unless $type =~ m/^\d$/;
  my $msg = join '', @_;
  unless ( defined($memo) ) {
    $memo = mClient::connect("memo");
  }
  my $hdr = '';
  if ($type == 1) {
    $hdr = "[WARNING] ";
  } elsif ($type == 2) {
    $hdr = "[ERROR] ";
  } elsif ($type == 3) {
    $hdr = "[FATAL] ";
  } elsif ($type > 3) {
    $hdr = "[INTERNAL] ";
  }
  my ($secs, $usecs) = gettimeofday;
  my @gmt = gmtime($secs);
  my $time = sprintf("%02d:%02d:%02d.%03d",
    $gmt[2], $gmt[1], $gmt[0], floor(($usecs+500)/1000);
  my $buf = "$time IFS: $hdr$msg\n";
  $memo->print($buf);
  exit(1) if $type > 2;
}

1;
