%INTERFACE <IFS>
%INTERFACE <IFSU>

%{
  #ifdef SERVER
  
  void IFSq_send( const char *cmd ) {
    if_IFS.Turf("%s\n", cmd);
  }

  #define CMDBUFSIZE 120
  void IFSq_scan( int repeat, const char *scantype, const char *params ) {
    char cmd[CMDBUFSIZE+1];
    int rv = snprintf( cmd, CMDBUFSIZE+1,
      "EQ%s%s %s", repeat ? "Repeat " : "",
      scantype, params );
    if ( rv >= CMDBUFSIZE ) {
      msg( 2, "IFS Scan command is too long" );
    } else {
      IFSq_send(cmd);
    }
  }

  #ifdef SKIP_FOR_TESTING  
  #include "resistors.h"
  #include "preamps.h"
  #endif
  
  void Set_Preamp_Gains( int InGaAs, int Si ) {
#ifdef SKIP_FOR_TESTING
    char cmd[10];
    FILE *fp;
    if ( InGaAs < 0 || InGaAs > 3 ) {
      msg( 2, "InGaAs index must be between 0 and 3" );
      return;
    }
    if ( Si < 0 || Si > 3 ) {
      msg( 2, "Si index must be between 0 and 3" );
      return;
    }
    IFSq_scan( 0, "Direct", IFS_SELECT_InGaAs );
    sprintf( cmd, "PGN=%d", InGaAs );
    IFSq_scan( 0, "Direct", cmd );
    IFSq_scan( 0, "Direct", IFS_SELECT_Si );
    sprintf( cmd, "PGN=%d", Si );
    IFSq_scan( 0, "Direct", cmd );
    msg( 0, "InGaAs preamp resistor set to %s", InGaAs_R[InGaAs] );
    msg( 0, "Si preamp resistor set to %s", Si_R[Si] );
    fp = fopen( "resistors.txt", "w" );
    if ( fp == 0 ) msg( 2, "Unable to write resistors.txt" );
    else {
      fprintf( fp, "InGaAs_R: %s\nSi_R: %s\n", InGaAs_R[InGaAs], Si_R[Si] );
      fprintf( fp, "R_Config_Index: %d\n", R_Config_Index );
      fclose( fp );
    }
#endif
  }
  

  #endif /* SERVER */
%}
&command
  : IFS Exit * { IFSq_send( "EQExit" ); }
  : IFS Repeat &ScanType %s (Enter Optional Scan Parameters) *
    { IFSq_scan( 1, $3, $4 ); }
  : IFS &ScanType %s (Enter Optional Scan Parameters) *
    { IFSq_scan( 0, $2, $3 ); }
  : IFS Direct %s (Enter Direct Parameters) *
    { IFSq_scan( 0, "Direct", $3 ); }
  : IFS Time Synch * { IFSq_send( "EQTime Synch" ); }
  : IFS Read Status * { IFSq_send( "EQRead Status" ); }
  : IFS Reset SW * { if_IFSU.Turf("EUReset\n"); }
  : IFS Set Preamp Gains %d (Enter Gain Index for InGaAs)
      %d (Enter Gain Index for Si) * { Set_Preamp_Gains( $5, $6 ); }
;
