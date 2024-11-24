package ColSend;
use strict;
use Fcntl;
use Errno;

sub Init {
  my ($name, $size) = @_;
  sysopen(my $fh, "/dev/huarp/citco2/DG/data/$name", O_NONBLOCK|O_WRONLY) ||
    die "Unable to open TM channel for '$name'\n";
  my $self = { name => $name, size => $size, fh => $fh };
  bless $self;
  return $self;
}

sub Send {
  my ($self, $data) = @_;
  my $rv = syswrite( $self->{fh}, $data, $self->{size} );
  if (!defined($rv) && $!{EAGAIN}) {
    warn "Colsend::Send() would block\n";
  } elsif ($rv != $self->{size}) {
    warn "Colsend::Send() incomplete write: $rv not $self->{size}\n"
      unless $rv == 0;
  }
}

1;
