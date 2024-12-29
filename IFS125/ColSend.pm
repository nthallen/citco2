package ColSend;
use strict;
use Fcntl;
use Errno;
use mClient;

sub Init {
  my ($name, $size) = @_;
  my $svc = mClient::connect("data", $name);
  my $self = { name => $name, size => $size, svc => $svc };
  bless $self;
  return $self;
}

sub Send {
  my ($self, $data) = @_;
  $self->{svc}->print($data);
  my $rv = syswrite( $self->{fh}, $data, $self->{size} );
  if (!defined($rv)) {
    if ($!{EAGAIN}) {
      warn "ColSend::Send() would block\n";
    } else {
      warn "ColSend::Send() returned error $!\n";
    }
  } elsif ($rv != $self->{size}) {
    warn "ColSend::Send() incomplete write: $rv not $self->{size}\n";
  }
}

1;
