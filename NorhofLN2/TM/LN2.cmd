%INTERFACE <LN2>

&command
  : LN2 Pump On * { if_LN2.Turf("P1\n"); }
  : LN2 Pump Off * { if_LN2.Turf("P0\n"); }
  : LN2 Quit * { if_LN2.Turf("Q\n"); }
  ;
