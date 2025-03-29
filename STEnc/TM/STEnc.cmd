%INTERFACE <STEnc>

&command
  : Enclosure &STEnc_Command * { if_STEnc.Turf("%s\n", $2); }
  : ASE DS-2C &ASEoverride &ASE_On_Off * { if_STEnc.Turf("S:%u\n", $4 ? $3 : 7); }
  ;
&STEnc_Command <const char *>
  : Open { $0 = "S:1"; }
  : Close { $0 = "S:0"; }
  : Relays Off { $0 = "S:2"; }
  : Exit { $0 = "Q"; }
  ;

&ASEoverride <int>
  : Manual { $0 = 5; }
  : Standby { $0 = 6; }
  ;

&ASE_On_Off <int>
  : On { $0 = 1; }
  : Off { $0 = 0; }
  ;
