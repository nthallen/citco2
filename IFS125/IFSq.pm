package IFSq;
use strict;
use mClient;
use IFSmsg qw(msg);
use Fcntl;
use Errno;

my $IFS_clt;
my $IFSU_clt;
sub IFSq_init {
  my $channel = shift;
  my $clt = mClient::connect("cmd", $channel);
  my $self = {
    queue = [],
    clt = $clt
  }
  return $self;
}

# non-blocking read. Actually the non-blocking part is
# handled on open.
# returns undef if there is nothing to read
sub IFSq_read {
  my $self = shift;
  my $buf;
  if ($self->{queue}) {
    $buf = pop(@{$self->{queue}});
  } else {
    $buf = $self->{clt}->read(500);
  }
  if (defined($buf) && $buf =~ s/^(.*\n)// ) {
    unshift(@{$self->{queue}}, $buf) if (length($buf));
    $buf = $1;
  }
# my $rv = sysread($fh,$buf,500);
# if ( defined $rv ) {
#   $buf = "EQExit\n" if $rv == 0;
# } else {
#   return undef if ( $!{EAGAIN} || $!{EWOULDBLOCK} );
#   msg( 3, "sysread returned an error" );
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
    my $IFS_fn = fileno($IFS_clt->{clt}->{sock});
    my $IFSU_fn = fileno($IFSU_clt->{clt}->{sock});
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
