package IFSctrl;
use strict;
use ColSend;
our %status;

my %TM = (
  IFSSlW => 0,
  IFS_P => 0,
  IFSScan => 0,
  IFSTR => 0,
  IFSRN => 0,
  IFSDT => 0,
  IFSSN => 0,
  IFSSR => 0,
  IFSCStat => 0,
  IFSDiag => 0,
  LasAAF => 0,
  LasAOF => 0,
  LasBAF => 0,
  LasBOF => 0,
  ScBlkT => 0,
  IFHum => 0,
  IFSSrcT => 0
);

my $colsend;
sub set_tm {
  my %vars = @_;
  for my $var ( keys %vars ) {
    $TM{$var} = $vars{$var};
  }
}

sub write_tm {
  my $data = pack( "LLSSsSsSssSsCCCCC", map $TM{$_},
      qw(IFSSlW IFS_P IFSTR IFSRN IFSDT LasAAF LasAOF LasBAF LasBOF
         ScBlkT IFHum IFSSrcT
	 IFSSN IFSSR IFSCStat IFSScan IFSDiag) );
  $colsend ||= ColSend::Init( "IFSCD", length($data) );
  $colsend->Send( $data );
}

sub set_status {
  my $statval = shift;
  IFSq_msg( 4, "Invalid statval '$statval'" )
    unless defined $status{$statval};
  set_tm( IFSCStat => $status{$statval} );
  write_tm();
}

1;
