%INTERFACE <SunTrack>

%{
  #ifdef SERVER
    #include <math.h>
    #include "SunTrack.h"
  #endif /* SERVER */
%}
&command
  : Sun Tracker &ST_Command * { if_SunTrack.Turf("%s\n", $3); }
# : Sun Tracker Goto %f (Azimuth Angle) %f (Elevation Angle) * {
#     double azimuth = $4;
#     double elevation = $5;
#     if ( azimuth < 0. || azimuth > 360. ) {
#       msg( 2, "Azimuth out of range" );
#     } else if ( elevation < -72 || elevation > 90 ) {
#       msg( 2, "Elevation out of range" );
#     } else {
#       unsigned short iazi = floor(azimuth*10);
#       signed short iele = floor(elevation*10);
#       if_SunTrack.Turf("%s%d,%d\n");
#     }
#   }
  ;
&ST_Command <const char *>
  : Track to Programmed Sun Position { $0 = "TS"; }
  : Track to Programmed Moon Position { $0 = "TM"; }
  : Track by CamTracker { $0 = "TR"; }
  : Track by Diode { $0 = "TD"; }
# : Cloud Detector Off { $0 = "C0"; }
# : Cloud Detector On { $0 = "C1"; }
  : Flip 0 { $0 = "F0"; }
  : Flip 1 { $0 = "F1"; }
  : Init { $0 = "IN"; }
  : Sleep { $0 = "SL"; }
  : Connect { $0 = "R"; }
  : Synchronize Time { $0 = "TY"; }
  : Exit { $0 = "Q"; }
  ;
