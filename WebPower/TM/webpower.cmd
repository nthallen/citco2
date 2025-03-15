%INTERFACE <webpower>

%{
  #ifdef SERVER
  #endif /* SERVER */
%}
&command
  : Power &wp_device &wp_onoff * { if_webpower.Turf("%s:%d\n", $3, $2); }
  : Power Quit { if_webpower.Turf("Q\n"); }
  ;

&wp_device <uint8_t>
  : Pump { $0 = 0; }
  : PDISO8 { $0 = 1; }
  : MKS925 { $0 = 2; }
  : WebCam { $0 = 3; }
  ;

&wp_onoff <const char *>
  : On { $0 = "N"; }
  : Off { $0 = "F"; }
  ;
