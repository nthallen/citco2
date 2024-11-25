package mClient;
use strict;
use IO::Socket::UNIX;
use IO::Select;

sub connect {
  my ($service, $subservice) = @_;
  $service =~ m|^\w+$| ||
    die "mClient::connect: Invalid service '$service'\n";
  if ($subservice &&
      $subservice !~ m|^\w+$|) {
    die "mClient::connect: Invalid subservice '$service'\n";
  }
  my $Experiment = $ENV{Experiment};
  $Experiment =~ m|^\w+$| ||
    die "mClient::connect: Invalid Experiment '$Experiment'\n";
  my $peer = "/var/run/monarch/$Experiment/$service";
  my $iname = $subservice ? "$service/$subservice" : $service;
  my $sock = IO::Socket::UNIX->new(
      Type => SOCK_STREAM,
      Peer => $peer,
      Blocking => 0
    ) || die "Unable to connect to $iname";
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
  $self->{sock}->syswrite($buf);
}

sub printf {
  my ($self, $fmt, @pfargs) = @_;
  my $buffer = sprintf($fmt, @pfargs);
  $self->{sock}->syswrite($buffer);
}

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
  my $rv = $self->{sock}->sysread($buffer, $length);
  warn $self->{iname} . ": Error $! on read\n"
    unless defined($rv);
  return $buffer;
}

1;
