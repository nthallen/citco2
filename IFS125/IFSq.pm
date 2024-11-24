package IFSctrl;
use strict;
use Fcntl;
use Errno;

my $IFS_fh;
my $IFSU_fh;
sub IFSq_init {
  my $node = shift;
  my $name = "/dev/huarp/citco2/cmd/$node";
  sysopen(my $fh, $name, O_NONBLOCK|O_RDONLY) ||
    die "Unable to open command channel '$name'\n";
  return $fh;
}

# non-blocking read. Actually the non-blocking part is
# handled on open.
sub IFSq_read {
  my $fh = shift;
  my $buf;
  my $rv = sysread($fh,$buf,500);
  if ( defined $rv ) {
    $buf = "EQExit\n" if $rv == 0;
  } else {
    return undef if ( $!{EAGAIN} || $!{EWOULDBLOCK} );
    IFSq_msg( 3, "sysread returned an error" );
  }
  return $buf;
}

# Takes either DQ, DW or DU args.
# DQ is non-blocking, and returns whatever is in either queue
# DU is non-blocking, and returns only commands from the IFSU queue
# DW blocks waiting for commands from either queue.
sub IFSq_dequeue {
  my $req = shift || "DQ";
  my $cmd;
  $IFS_fh = IFSq_init("IFS") unless defined $IFS_fh;
  $IFSU_fh = IFSq_init("IFSU") unless defined $IFSU_fh;
  if ($req eq "DQ") {
    $cmd = IFSq_read($IFSU_fh);
    $cmd = IFSq_read($IFS_fh) unless defined $cmd;
  } elsif ($req eq "DU") {
    $cmd = IFSq_read($IFSU_fh);
  } elsif ($req eq "DW") {
    my $rin = '';
    vec($rin,fileno($IFS_fh),1) = 1;
    vec($rin,fileno($IFSU_fh),1) = 1;
    my $nfound = select($rin,undef,undef,undef);
    if ($nfound) {
      if ( vec($rin,fileno($IFSU_fh),1) ) {
        $cmd = IFSq_read($IFSU_fh);
      } elsif ( vec($rin,fileno($IFS_fh),1) ) {
        $cmd = IFSq_read($IFS_fh);
      }
    }
  }
  $cmd = "EQ" unless defined $cmd; 
  return $cmd;
}

sub IFSq_dequeue_wait {
  return IFSq_dequeue( "DW" );
}

sub IFSq_dequeue_urgent {
  return IFSq_dequeue( "DU" );
}

my $msg_fh;

sub IFSq_msg {
  my $type = shift;
  $type = 2 unless $type =~ m/^\d$/;
  my $msg = join '', @_;
  unless ( defined($msg_fh) ) {
    sysopen($msg_fh, "/dev/huarp/citco2/memo", O_WRONLY) ||
      die "Unable to open memo channel\n";
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
  my @gmt = gmtime;
  my $time = sprintf("IFS: %02d:%02d:%02d", $gmt[2], $gmt[1], $gmt[0]);
  my $buf = "$time $hdr$msg\n";
  syswrite($msg_fh, $buf, length($buf));
  exit(1) if $type > 2;
}

1;
