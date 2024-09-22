%INTERFACE <STEnc>

&command
  : Enclosure &STEnc_Command * { if_STEnc.Turf("%s\n", $2); }
  ;
&STEnc_Command <const char *>
  : Open { $0 = "S:1"; }
  : Close { $0 = "S:0"; }
  : Relays Off { $0 = "S:2"; }
  : Exit { $0 = "Q"; }
  ;
