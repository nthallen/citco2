package IFSq;
use strict;
use mClient;
use Fcntl;
use Errno;

my $IFS_clt;
my $IFSU_clt;
sub IFSq_init {
  my $channel = shift;
  my $clt = mClient::connect("cmd", $channel);
  return $clt;
}

# non-blocking read. Actually the non-blocking part is
# handled on open.
# returns undef if there is nothing to read
sub IFSq_read {
  my $clt = shift;
  my $buf = $clt->read(500);
# my $rv = sysread($fh,$buf,500);
# if ( defined $rv ) {
#   $buf = "EQExit\n" if $rv == 0;
# } else {
#   return undef if ( $!{EAGAIN} || $!{EWOULDBLOCK} );
#   IFSq_msg( 3, "sysread returned an error" );
# }
  $buf = "EQExit\n" if defined($buf) && $buf =~ m/^Q?$/;
  return $buf;
}

# Takes either DQ, DW or DU args.
# DQ is non-blocking, and returns whatever is in either queue
# DU is non-blocking, and returns only commands from the IFSU queue
# DW blocks waiting for commands from either queue.
sub IFSq_dequeue {
  my $req = shift || "DQ";
  my $cmd;
  $IFS_clt ||= IFSq_init("IFS");
  $IFSU_clt ||= IFSq_init("IFSU");
  if ($req eq "DQ") {
    $cmd = IFSq_read($IFSU_clt);
    $cmd ||= IFSq_read($IFS_clt);
  } elsif ($req eq "DU") {
    $cmd = IFSq_read($IFSU_clt);
  } elsif ($req eq "DW") {
    my $IFS_fn = fileno($IFS_clt->{sock});
    my $IFSU_fn = fileno($IFSU_clt->{sock});
    my $rin = '';
    vec($rin,$IFS_fn,1) = 1;
    vec($rin,$IFSU_fn,1) = 1;
    my $nfound = select($rin,undef,undef,undef);
    if ($nfound) {
      if ( vec($rin,$IFSU_fn,1) ) {
        $cmd = IFSq_read($IFSU_clt);
      } elsif ( vec($rin,$IFS_fn,1) ) {
        $cmd = IFSq_read($IFS_clt);
      }
    }
  }
  $cmd ||= "EQ";
  return $cmd;
}

sub IFSq_dequeue_wait {
  return IFSq_dequeue( "DW" );
}

sub IFSq_dequeue_urgent {
  return IFSq_dequeue( "DU" );
}

1;
