#! /usr/bin/perl -w
package IFSUserAgent;
use strict;
use parent qw(LWP::UserAgent);
use IFSUAconfig;

our %credentials;

sub new {
  my ($class, @opts) = @_;
  my $ua = new LWP::UserAgent(@opts);
  bless $ua, $class;
}


sub get_basic_credentials {
  my ($self, $realm, $url) = @_;
  if ($credentials{$url}) {
    my $cr = $credentials{$url};
    return ($cr->{user}, $cr->{password});
    # if ($url eq $jpg_url) {
    #   return ('viewer', 'viewer');
    # }
  }
  return undef;
}

1;
