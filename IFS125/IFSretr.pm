package IFSretr;
use strict;
use HTTP::Request::Common qw(GET);
use LWP::UserAgent;
use ColSend;

our $ua;
our $server;

sub init {
  $server = shift;
  $| = 1;
  $ua = new LWP::UserAgent;
  open(RETRLOG, ">>IFSretr.log" ) || die "Cannot log to IFSretr.log\n";
  select RETRLOG;
  $| = 1;
  select STDOUT;

  -d 'scan' || mkdir('scan') ||
     die "Cannot create scan directory\n";
  print RETRLOG scalar(gmtime), ": IFSretr.pm started, server is $server\n";
}

my $colsend;
my $nextfile;
my $slicesize;

sub line {
  my $line = shift;
  chomp $line;
  my $now = gmtime;
  if ( $line =~ m/^b(\d+)\.0$/ ) {
    my $curfile = $1;
    $nextfile = $curfile if ! defined($nextfile);
    while ( $nextfile <= $curfile ) {
      print RETRLOG "$now: Retrieving b$nextfile.0: ";
      my $slice_start_time = time;
      retrieve( $nextfile );
      my $slice_time = time - $slice_start_time;
      if ( $slicesize ) {
        if ( $slice_time ) {
          my $slice_rate = ($slicesize/$slice_time)/1000;
          printf RETRLOG "%d in %d secs \@ %.1f KB/s\n", $slicesize, $slice_time, $slice_rate;
        } else {
          print RETRLOG "$slicesize bytes instantly\n";
        }
      } else {
        print RETRLOG "$slice_time sec\n";
      }
      $nextfile++;
    }
  } elsif ( $line =~ m/^#(.*)$/ ) {
    print RETRLOG "$now: $1\n";
  } else {
    print RETRLOG "$now: Received bad data: '$line'\n";
  }
}

END {
  if ( $server ) {
    print RETRLOG scalar(gmtime), ": IFSretr exiting\n\n";
    close RETRLOG;
  }
}

sub dummy_cb {
  my ( $data, $resp, $proto ) = @_;
  $slicesize += length($data);
}

# Return TRUE on success
sub retrieve {
  my ( $filenum ) = @_;
  my $filename = "b$filenum.0";
  my $furi = "$server/$filename";
  $slicesize = 0;
  
  #---New syntax:
  my $response = $ua->get( $furi, ':content_file' => "scan/$filename" );
  #---Test w/o writing to disk:
  # my $response = $ua->get( $furi, ':content_cb' => \&dummy_cb, ':read_size_hint' => 10240 );

  write_tm( $filenum, $response->code );
  if ( $response->is_success ) {
    return 1;
  } else {
    print RETRLOG "  Failure: ", $response->status_line, "\n";
    return 0;
  }
}

sub write_tm {
  my ( $IFSSlR, $IFSRStat ) = @_;
  my $data = pack( "LS", $IFSSlR, $IFSRStat );
  $colsend = ColSend::Init( "IFSRD", length($data) )
    unless defined $colsend;
  $colsend->Send( $data );
}

1;
