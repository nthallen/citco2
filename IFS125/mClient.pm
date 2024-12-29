package mClient;
use strict;
use IO::Socket::UNIX;
use IO::Select;
use Fcntl qw(F_GETFL F_SETFL O_NONBLOCK);
use Errno;

sub connect {
  my ($service, $subservice) = @_;
  $service =~ m|^\w+$| ||
    die "mClient::connect: Invalid service '$service'\n";
  if ($subservice &&
      $subservice !~ m|^[\w/]+$|) {
    die "mClient::connect: Invalid subservice '$service'\n";
  }
  my $Experiment = $ENV{Experiment} || "none";
  $Experiment =~ m|^\w+$| ||
    die "mClient::connect: Invalid Experiment '$Experiment'\n";
  my $peer = "/var/run/monarch/$Experiment/$service";
  my $iname = $subservice ? "$service/$subservice" : $service;
  # Blocking set to 1 here because the non-blocking
  # connect doesn't work on Cygwin.
  my $sock = IO::Socket::UNIX->new(
      Type => SOCK_STREAM,
      Peer => $peer,
      Blocking => 1
    ) || die "Unable to connect to $iname socket $peer\n";

  # This works around the connect() problem on Cygwin
  my $flags = fcntl($sock, F_GETFL, 0)
    or die "Cannot get flags for socket $!\n";
  fcntl($sock, F_SETFL, $flags | O_NONBLOCK)
    or die "Cannot set flags for socket $!\n";
  my $sel = IO::Select->new($sock);

  my $self = {
    iname => $iname,
    sock => $sock,
    sel => $sel
  };
  bless $self;
  
  my $Appname = "IFS";
  $self->printf("AuthLE %s %s %s\n",
    $Experiment, $Appname, $iname);
  my $resp = $self->read(10, 1); # read 10 with 1sec timeout
  die "$iname: Authentication failed\n"
    if $resp ne "OK\n";
  return $self;
}

sub print {
  my ($self, $buf) = @_;
  return $self->{sock}->syswrite($buf);
}

sub printf {
  my ($self, $fmt, @pfargs) = @_;
  my $buffer = sprintf($fmt, @pfargs);
  $self->{sock}->syswrite($buffer);
}

# return undef if nothing to read
sub read {
  my ($self, $length, $timeout) = @_;
  if ($timeout) {
    $! = 0;
    my @ready = $self->{sel}->can_read($timeout);
    unless (@ready) {
      if ($!) {
        die $self->{iname} . ": Error $! on select\n";
      } else {
        return ""; # timeout
      }
    }
  }
  my $buffer = "";
  $! = 0;
  my $rv = $self->{sock}->sysread($buffer, $length);
  if (! defined $rv) {
    return undef if ( $!{EAGAIN} || $!{EWOULDBLOCK});
    msg(3, $self->{iname} . ": Error $! on read");
  }
  return $buffer;
}

1;
